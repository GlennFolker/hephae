use std::{
    marker::PhantomData,
    ops::Range,
    sync::{PoisonError, RwLock},
};

use bevy::{
    core_pipeline::{
        core_2d::Transparent2d,
        tonemapping::{get_lut_bind_group_layout_entries, get_lut_bindings, DebandDither, Tonemapping, TonemappingLuts},
    },
    ecs::{
        query::ROQueryItem,
        system::{
            lifetimeless::{Read, SRes},
            ReadOnlySystemParam, SystemParamItem, SystemState,
        },
    },
    math::FloatOrd,
    prelude::*,
    render::{
        mesh::PrimitiveTopology,
        render_asset::RenderAssets,
        render_phase::{
            DrawFunctions, PhaseItem, PhaseItemExtraIndex, RenderCommand, RenderCommandResult, SetItemPipeline,
            TrackedRenderPass, ViewSortedRenderPhases,
        },
        render_resource::{
            binding_types::uniform_buffer, BindGroup, BindGroupEntry, BindGroupLayout, BindingResource, BlendState,
            BufferAddress, BufferUsages, ColorTargetState, ColorWrites, FragmentState, FrontFace, IndexFormat,
            MultisampleState, PipelineCache, PolygonMode, PrimitiveState, RawBufferVec, RenderPipelineDescriptor,
            ShaderDefVal, ShaderStages, SpecializedRenderPipeline, SpecializedRenderPipelines, TextureFormat,
            VertexBufferLayout, VertexState, VertexStepMode,
        },
        renderer::{RenderDevice, RenderQueue},
        texture::{BevyDefault, FallbackImage, GpuImage},
        view::{ExtractedView, ViewTarget, ViewUniform, ViewUniformOffset, ViewUniforms},
        Extract,
    },
    utils::{EntityHash, EntityHashMap, EntityHashSet},
};
use dashmap::DashMap;
use fixedbitset::FixedBitSet;

use crate::vertex::{Vertex, VertexCommand, VertexQueuer};

#[derive(Resource)]
pub struct HephaePipeline<T: Vertex> {
    view_layout: BindGroupLayout,
    vertex_prop: T::PipelineProp,
}

impl<T: Vertex> FromWorld for HephaePipeline<T> {
    fn from_world(world: &mut World) -> Self {
        let device = world.resource::<RenderDevice>();

        let [lut_texture, lut_sampler] = get_lut_bind_group_layout_entries();
        let view_layout = device.create_bind_group_layout(
            "hephae_view_layout",
            &[
                uniform_buffer::<ViewUniform>(true).build(0, ShaderStages::VERTEX_FRAGMENT),
                lut_texture.build(1, ShaderStages::FRAGMENT),
                lut_sampler.build(2, ShaderStages::FRAGMENT),
            ],
        );

        let mut state = SystemState::<T::PipelineParam>::new(world);
        let vertex_prop = T::init_pipeline(state.get_mut(world));
        state.apply(world);

        Self {
            view_layout,
            vertex_prop,
        }
    }
}

#[derive(Resource)]
pub struct PipelineShader<T: Vertex>(pub(crate) Handle<Shader>, PhantomData<fn() -> T>);
impl<T: Vertex> PipelineShader<T> {
    #[inline]
    pub fn shader(&self) -> AssetId<Shader> {
        self.0.id()
    }
}

pub fn load_shader<T: Vertex>(mut commands: Commands, server: Res<AssetServer>) {
    commands.insert_resource(PipelineShader::<T>(server.load(T::SHADER), PhantomData));
}

pub fn extract_shader<T: Vertex>(mut commands: Commands, shader: Extract<Option<Res<PipelineShader<T>>>>) {
    if let Some(ref shader) = *shader {
        if shader.is_changed() {
            commands.insert_resource(PipelineShader::<T>(shader.0.clone_weak(), PhantomData));
        }
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct ViewKey {
    hdr: bool,
    msaa: u8,
    tonemapping: Option<Tonemapping>,
    dither: bool,
    shader: Handle<Shader>,
}

impl Clone for ViewKey {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            shader: self.shader.clone_weak(),
            ..*self
        }
    }
}

impl<T: Vertex> SpecializedRenderPipeline for HephaePipeline<T> {
    type Key = (ViewKey, T::PipelineKey);

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let (view_key, key) = key;
        let mut defs = Vec::new();
        if let Some(tonemapping) = view_key.tonemapping {
            defs.extend([
                "TONEMAP_IN_SHADER".into(),
                ShaderDefVal::UInt("TONEMAPPING_LUT_TEXTURE_BINDING_INDEX".into(), 1),
                ShaderDefVal::UInt("TONEMAPPING_LUT_SAMPLER_BINDING_INDEX".into(), 2),
                match tonemapping {
                    Tonemapping::None => "TONEMAP_METHOD_NONE",
                    Tonemapping::Reinhard => "TONEMAP_METHOD_REINHARD",
                    Tonemapping::ReinhardLuminance => "TONEMAP_METHOD_REINHARD_LUMINANCE",
                    Tonemapping::AcesFitted => "TONEMAP_METHOD_ACES_FITTED",
                    Tonemapping::AgX => "TONEMAP_METHOD_AGX",
                    Tonemapping::SomewhatBoringDisplayTransform => "TONEMAP_METHOD_SOMEWHAT_BORING_DISPLAY_TRANSFORM",
                    Tonemapping::TonyMcMapface => "TONEMAP_METHOD_TONY_MC_MAPFACE",
                    Tonemapping::BlenderFilmic => "TONEMAP_METHOD_BLENDER_FILMIC",
                }
                .into(),
            ]);

            if view_key.dither {
                defs.push("DEBAND_DITHER".into());
            }
        }

        let format = match view_key.hdr {
            true => ViewTarget::TEXTURE_FORMAT_HDR,
            false => TextureFormat::bevy_default(),
        };

        let mut desc = RenderPipelineDescriptor {
            label: Some("hephae_pipeline_descriptor".into()),
            layout: [self.view_layout.clone()].into(),
            push_constant_ranges: Vec::new(),
            vertex: VertexState {
                shader: view_key.shader.clone_weak(),
                shader_defs: defs.clone(),
                entry_point: "vertex".into(),
                buffers: [VertexBufferLayout {
                    array_stride: size_of::<T>() as BufferAddress,
                    step_mode: VertexStepMode::Vertex,
                    attributes: T::LAYOUT.into(),
                }]
                .into(),
            },
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: None,
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
            },
            depth_stencil: None,
            multisample: MultisampleState {
                count: 1 << view_key.msaa,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            fragment: Some(FragmentState {
                shader: view_key.shader,
                shader_defs: defs,
                entry_point: "fragment".into(),
                targets: [Some(ColorTargetState {
                    format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })]
                .into(),
            }),
        };

        T::specialize_pipeline(key, &self.vertex_prop, &mut desc);
        desc
    }
}

#[derive(Resource)]
pub struct VertexQueues<T: Vertex> {
    pub(crate) commands: DashMap<Entity, Vec<(f32, T::PipelineKey, T::Command)>, EntityHash>,
    pub(crate) entities: DashMap<Entity, EntityHashSet<Entity>, EntityHash>,
    pub(crate) entity_bits: RwLock<FixedBitSet>,
}

impl<T: Vertex> Default for VertexQueues<T> {
    #[inline]
    fn default() -> Self {
        Self {
            commands: default(),
            entities: default(),
            entity_bits: default(),
        }
    }
}

pub fn queue_batches<T: Vertex>(
    mut batches: ResMut<HephaeBatches<T>>,
    views: Query<Entity, With<ExtractedView>>,
    mut all_views: Local<FixedBitSet>,
) {
    all_views.clear();
    for view in &views {
        all_views.grow_and_insert(view.index() as usize);
        let (vertices, indices) = batches.0.entry(view).or_insert((
            RawBufferVec::new(BufferUsages::VERTEX),
            RawBufferVec::new(BufferUsages::INDEX),
        ));

        vertices.clear();
        indices.clear();
    }

    batches.0.retain(|&view, _| all_views.contains(view.index() as usize));
}

pub fn queue_vertices<T: Vertex>(
    mut queues: ResMut<VertexQueues<T>>,
    msaa: Res<Msaa>,
    draw_functions: Res<DrawFunctions<Transparent2d>>,
    pipeline: Res<HephaePipeline<T>>,
    shader: Res<PipelineShader<T>>,
    mut pipelines: ResMut<SpecializedRenderPipelines<HephaePipeline<T>>>,
    mut pipeline_cache: ResMut<PipelineCache>,
    mut transparent_phases: ResMut<ViewSortedRenderPhases<Transparent2d>>,
    views: Query<(Entity, &ExtractedView, Option<&Tonemapping>, Option<&DebandDither>)>,
) where
    <T::RenderCommand as RenderCommand<Transparent2d>>::Param: ReadOnlySystemParam,
{
    let queues = &mut *queues;
    let draw_function = draw_functions.read().id::<DrawRequests<T>>();
    let msaa = msaa.samples().trailing_zeros() as u8;

    for (view_entity, view, tonemapping, dither) in &views {
        let Some(transparent_phase) = transparent_phases.get_mut(&view_entity) else {
            continue;
        };

        let view_key = ViewKey {
            hdr: view.hdr,
            msaa,
            tonemapping: (!view.hdr).then_some(tonemapping.copied()).flatten(),
            dither: !view.hdr && dither.copied().unwrap_or_default() == DebandDither::Enabled,
            shader: shader.0.clone_weak(),
        };

        let Some(mut entities) = queues.entities.get_mut(&view_entity) else {
            continue;
        };

        for e in entities.drain() {
            let Some(commands) = queues.commands.get(&e) else { continue };
            for (i, &(layer, ref key, ..)) in commands.iter().enumerate() {
                transparent_phase.add(Transparent2d {
                    sort_key: FloatOrd(layer),
                    entity: e,
                    pipeline: pipelines.specialize(&mut pipeline_cache, &pipeline, (view_key.clone(), key.clone())),
                    draw_function,
                    batch_range: 0..0,
                    extra_index: PhaseItemExtraIndex(i as u32),
                });
            }
        }
    }

    let bits = queues.entity_bits.get_mut().unwrap_or_else(PoisonError::into_inner);
    queues.commands.retain(|&e, _| bits.contains(e.index() as usize));
    queues.entities.iter_mut().for_each(|mut entities| entities.clear());
    bits.clear();
}

#[derive(Component)]
pub struct HephaeViewBindGroup<T: Vertex>(BindGroup, PhantomData<fn() -> T>);

#[derive(Resource)]
pub struct HephaeBatches<T: Vertex>(EntityHashMap<Entity, (RawBufferVec<T>, RawBufferVec<u32>)>);
impl<T: Vertex> Default for HephaeBatches<T> {
    #[inline]
    fn default() -> Self {
        Self(default())
    }
}

#[derive(Component)]
pub struct HephaeBatch<T: Vertex> {
    prop: T::BatchProp,
    range: Range<u32>,
}

impl<T: Vertex> HephaeBatch<T> {
    #[inline]
    pub fn prop(&self) -> &T::BatchProp {
        &self.prop
    }

    #[inline]
    pub fn range(&self) -> &Range<u32> {
        &self.range
    }
}

pub fn prepare_batch<T: Vertex>(
    mut param_set: ParamSet<(
        (
            Res<VertexQueues<T>>,
            Res<RenderDevice>,
            Res<RenderQueue>,
            ResMut<HephaeBatches<T>>,
            ResMut<ViewSortedRenderPhases<Transparent2d>>,
            Query<Entity, With<ExtractedView>>,
        ),
        T::BatchParam,
        Commands,
    )>,
    mut batched_entities: Local<Vec<(Entity, T::PipelineKey, Range<u32>)>>,
    mut batched_results: Local<Vec<(Entity, HephaeBatch<T>)>>,
) {
    struct Queuer<'a, T: Vertex> {
        len: u32,
        vertices: &'a mut Vec<T>,
        indices: &'a mut Vec<u32>,
    }

    impl<'a, T: Vertex> VertexQueuer for Queuer<'a, T> {
        type Vertex = T;

        #[inline]
        fn vertices(&mut self, vertices: impl IntoIterator<Item = Self::Vertex>) {
            self.vertices.extend(vertices);
        }

        #[inline]
        fn indices(&mut self, indices: impl IntoIterator<Item = u32>) {
            self.indices.extend(indices.into_iter().map(|index| index + self.len));
        }
    }

    let (queues, render_device, render_queue, mut batches, mut transparent_phases, views) = param_set.p0();
    for view in &views {
        let Some(transparent_phase) = transparent_phases.get_mut(&view) else {
            continue;
        };

        let Some((vertices, indices)) = batches.0.get_mut(&view) else {
            continue;
        };

        let mut batch_item_index = 0;
        let mut batch_range = 0;
        let mut batch_key = None::<T::PipelineKey>;

        let mut queuer = Queuer {
            len: 0,
            vertices: vertices.values_mut(),
            indices: indices.values_mut(),
        };

        for item_index in 0..transparent_phase.items.len() {
            let item = &mut transparent_phase.items[item_index];
            let Some(commands) = queues.commands.get(&item.entity) else {
                batch_key = None;
                continue;
            };

            let Some((.., ref key, ref command)) =
                commands.get(std::mem::replace(&mut item.extra_index, PhaseItemExtraIndex::NONE).0 as usize)
            else {
                continue;
            };

            command.draw(&mut queuer);
            queuer.len += queuer.vertices.len() as u32;

            if match batch_key {
                None => true,
                Some(ref batch_key) => batch_key == key,
            } {
                batch_item_index = item_index;
                batched_entities.push((item.entity, key.clone(), batch_range..batch_range));
            }

            batch_range += queuer.indices.len() as u32;
            transparent_phase.items[batch_item_index].batch_range.end += 1;
            batched_entities.last_mut().unwrap().2.end = batch_range;
        }

        vertices.write_buffer(&render_device, &render_queue);
        indices.write_buffer(&render_device, &render_queue);
    }

    queues.commands.iter_mut().for_each(|mut commands| commands.clear());
    batched_results.reserve(batched_entities.len());

    let mut param = param_set.p1();
    for (batch_entity, key, range) in batched_entities.drain(..) {
        batched_results.push((
            batch_entity,
            HephaeBatch {
                prop: T::create_batch(&mut param, key),
                range,
            },
        ));
    }

    drop(param);

    let mut commands = param_set.p2();
    for (batch_entity, batch) in batched_results.drain(..) {
        commands.get_or_spawn(batch_entity).insert(batch);
    }
}

pub fn prepare_view_bind_groups<T: Vertex>(
    mut commands: Commands,
    pipeline: Res<HephaePipeline<T>>,
    render_device: Res<RenderDevice>,
    view_uniforms: Res<ViewUniforms>,
    views: Query<(Entity, &Tonemapping), With<ExtractedView>>,
    tonemapping_luts: Res<TonemappingLuts>,
    images: Res<RenderAssets<GpuImage>>,
    fallback_image: Res<FallbackImage>,
) {
    let Some(view_binding) = view_uniforms.uniforms.binding() else {
        return;
    };

    for (entity, &tonemapping) in &views {
        let (lut_texture, lut_sampler) = get_lut_bindings(&images, &tonemapping_luts, &tonemapping, &fallback_image);
        let view_bind_group = render_device.create_bind_group(
            "hephae_view_bind_group",
            &pipeline.view_layout,
            &[
                BindGroupEntry {
                    binding: 0,
                    resource: view_binding.clone(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::TextureView(lut_texture),
                },
                BindGroupEntry {
                    binding: 2,
                    resource: BindingResource::Sampler(lut_sampler),
                },
            ],
        );

        commands
            .entity(entity)
            .insert(HephaeViewBindGroup::<T>(view_bind_group, PhantomData));
    }
}

pub type DrawRequests<T> = (
    SetItemPipeline,
    SetHephaeViewBindGroup<T, 0>,
    <T as Vertex>::RenderCommand,
    DrawBatch<T>,
);

pub struct SetHephaeViewBindGroup<T: Vertex, const I: usize>(PhantomData<fn() -> T>);
impl<P: PhaseItem, T: Vertex, const I: usize> RenderCommand<P> for SetHephaeViewBindGroup<T, I> {
    type Param = ();
    type ViewQuery = (Read<ViewUniformOffset>, Read<HephaeViewBindGroup<T>>);
    type ItemQuery = ();

    #[inline]
    fn render<'w>(
        _: &P,
        (view_uniform, view_bind_group): ROQueryItem<'w, Self::ViewQuery>,
        _: Option<ROQueryItem<'w, Self::ItemQuery>>,
        _: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        pass.set_bind_group(I, &view_bind_group.0, &[view_uniform.offset]);
        RenderCommandResult::Success
    }
}

pub struct DrawBatch<T: Vertex>(PhantomData<fn() -> T>);
impl<P: PhaseItem, T: Vertex> RenderCommand<P> for DrawBatch<T> {
    type Param = SRes<HephaeBatches<T>>;
    type ViewQuery = Entity;
    type ItemQuery = Read<HephaeBatch<T>>;

    #[inline]
    fn render<'w>(
        _: &P,
        view: ROQueryItem<'w, Self::ViewQuery>,
        entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        batches: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(HephaeBatch { range, .. }) = entity else {
            return RenderCommandResult::Failure;
        };

        let Some((vertices, indices)) = batches.into_inner().0.get(&view) else {
            return RenderCommandResult::Failure;
        };

        pass.set_vertex_buffer(0, vertices.buffer().unwrap().slice(..));
        pass.set_index_buffer(indices.buffer().unwrap().slice(..), 0, IndexFormat::Uint32);
        pass.draw_indexed(range.clone(), 0, 0..1);

        RenderCommandResult::Success
    }
}
