use std::mem::offset_of;

use bevy::{
    core_pipeline::bloom::BloomSettings,
    ecs::{
        query::{QueryItem, ROQueryItem},
        system::{
            lifetimeless::{Read, SRes, SResMut},
            SystemParamItem,
        },
    },
    prelude::*,
    render::{
        render_asset::RenderAssets,
        render_phase::{PhaseItem, RenderCommand, RenderCommandResult, TrackedRenderPass},
        render_resource::{
            binding_types::{sampler, texture_2d},
            BindGroup, BindGroupEntry, BindGroupLayout, BufferAddress, IntoBinding, RenderPipelineDescriptor,
            SamplerBindingType, ShaderStages, TextureSampleType, VertexAttribute, VertexFormat,
        },
        renderer::RenderDevice,
        texture::GpuImage,
        Extract, Render, RenderApp,
    },
    utils::HashMap,
};
use hephae::prelude::*;

#[derive(Resource, Default)]
struct ImageAssetEvents(Vec<AssetEvent<Image>>);

#[derive(Resource, Default)]
struct ImageBindGroups(HashMap<AssetId<Image>, BindGroup>);

fn extract_image_events(mut images: ResMut<ImageAssetEvents>, mut events: Extract<EventReader<AssetEvent<Image>>>) {
    let images = &mut images.0;
    images.extend(events.read());
}

fn validate_image_bind_groups(mut image_bind_groups: ResMut<ImageBindGroups>, mut events: ResMut<ImageAssetEvents>) {
    for event in events.0.drain(..) {
        match event {
            AssetEvent::Added { .. } | AssetEvent::LoadedWithDependencies { .. } => {}
            AssetEvent::Modified { id } | AssetEvent::Removed { id } | AssetEvent::Unused { id } => {
                image_bind_groups.0.remove(&id);
            }
        }
    }
}

#[derive(Copy, Clone, Pod, Zeroable)]
#[repr(C)]
struct SpriteVertex {
    pos: [f32; 2],
    uv: [f32; 2],
}

impl SpriteVertex {
    #[inline]
    pub const fn new(x: f32, y: f32, u: f32, v: f32) -> Self {
        Self { pos: [x, y], uv: [u, v] }
    }
}

impl Vertex for SpriteVertex {
    type PipelineParam = SRes<RenderDevice>;
    type PipelineProp = BindGroupLayout;
    type PipelineKey = AssetId<Image>;

    type Command = Sprite;

    type BatchParam = (
        SRes<RenderDevice>,
        SRes<RenderAssets<GpuImage>>,
        SRes<HephaePipeline<Self>>,
        SResMut<ImageBindGroups>,
    );
    type BatchProp = AssetId<Image>;

    type RenderCommand = SetSpriteBindGroup<1>;

    const SHADER: &'static str = "sprite.wgsl";
    const LAYOUT: &'static [VertexAttribute] = &[
        VertexAttribute {
            format: VertexFormat::Float32x2,
            offset: offset_of!(Self, pos) as BufferAddress,
            shader_location: 0,
        },
        VertexAttribute {
            format: VertexFormat::Float32x2,
            offset: offset_of!(Self, uv) as BufferAddress,
            shader_location: 1,
        },
    ];

    #[inline]
    fn setup(app: &mut App) {
        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .init_resource::<ImageAssetEvents>()
                .init_resource::<ImageBindGroups>()
                .add_systems(ExtractSchedule, extract_image_events)
                .add_systems(
                    Render,
                    validate_image_bind_groups.before_ignore_deferred(HephaeSystems::PrepareBindGroups),
                );
        }
    }

    #[inline]
    fn init_pipeline(render_device: SystemParamItem<Self::PipelineParam>) -> Self::PipelineProp {
        render_device.create_bind_group_layout(
            "sprite_material_layout",
            &[
                texture_2d(TextureSampleType::Float { filterable: true }).build(0, ShaderStages::FRAGMENT),
                sampler(SamplerBindingType::Filtering).build(1, ShaderStages::FRAGMENT),
            ],
        )
    }

    #[inline]
    fn specialize_pipeline(
        _: Self::PipelineKey,
        material_bind_group: &Self::PipelineProp,
        desc: &mut RenderPipelineDescriptor,
    ) {
        desc.layout.push(material_bind_group.clone());
    }

    #[inline]
    fn create_batch(
        (ref render_device, ref gpu_images, ref pipeline, image_bind_groups): &mut SystemParamItem<Self::BatchParam>,
        key: Self::PipelineKey,
    ) -> Self::BatchProp {
        let Some(gpu_image) = gpu_images.get(key) else { return key };
        image_bind_groups.0.entry(key).or_insert_with(|| {
            render_device.create_bind_group(
                "sprite_material",
                pipeline.vertex_prop(),
                &[
                    BindGroupEntry {
                        binding: 0,
                        resource: gpu_image.texture_view.into_binding(),
                    },
                    BindGroupEntry {
                        binding: 1,
                        resource: gpu_image.sampler.into_binding(),
                    },
                ],
            )
        });

        key
    }
}

struct SetSpriteBindGroup<const I: usize>;
impl<P: PhaseItem, const I: usize> RenderCommand<P> for SetSpriteBindGroup<I> {
    type Param = SRes<ImageBindGroups>;
    type ViewQuery = ();
    type ItemQuery = Read<HephaeBatch<SpriteVertex>>;

    #[inline]
    fn render<'w>(
        _: &P,
        _: ROQueryItem<'w, Self::ViewQuery>,
        batch: Option<ROQueryItem<'w, Self::ItemQuery>>,
        image_bind_groups: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let image_bind_groups = image_bind_groups.into_inner();
        let Some(batch) = batch else {
            return RenderCommandResult::Failure;
        };

        pass.set_bind_group(I, &image_bind_groups.0[batch.prop()], &[]);
        RenderCommandResult::Success
    }
}

#[derive(Component, Copy, Clone)]
struct DrawSprite {
    pos: Vec2,
    scl: Vec2,
    page: AssetId<Image>,
    rect: URect,
}

impl Drawer for DrawSprite {
    type Vertex = SpriteVertex;

    type ExtractParam = SRes<Assets<TextureAtlas>>;
    type ExtractData = (Read<GlobalTransform>, Read<Handle<TextureAtlas>>, Read<AtlasIndex>);
    type ExtractFilter = ();

    type DrawParam = SRes<RenderAssets<GpuImage>>;

    #[inline]
    fn extract(
        atlases: &SystemParamItem<Self::ExtractParam>,
        (&trns, atlas, &index): QueryItem<Self::ExtractData>,
    ) -> Option<Self> {
        let atlas = atlases.get(atlas)?;
        let (page_index, rect_index) = index.indices()?;

        let (page, rect) = atlas
            .pages
            .get(page_index)
            .and_then(|page| Some((page.image.id(), *page.sprites.get(rect_index)?)))?;

        let (scale, .., translation) = trns.to_scale_rotation_translation();
        Some(DrawSprite {
            pos: translation.truncate(),
            scl: scale.truncate(),
            page,
            rect,
        })
    }

    #[inline]
    fn enqueue(
        &self,
        images: &SystemParamItem<Self::DrawParam>,
        queuer: &mut impl Extend<(f32, <Self::Vertex as Vertex>::PipelineKey, <Self::Vertex as Vertex>::Command)>,
    ) {
        let Some(page) = images.get(self.page) else { return };

        let Vec2 { x, y } = self.pos;
        let Vec2 { x: hw, y: hh } = (self.rect.max - self.rect.min).as_vec2() / 2.0 * self.scl;
        let Vec2 { x: u, y: v2 } = self.rect.min.as_vec2() / page.size.as_vec2();
        let Vec2 { x: u2, y: v } = self.rect.max.as_vec2() / page.size.as_vec2();

        queuer.extend([(
            0.0,
            self.page,
            Sprite {
                x,
                y,
                hw,
                hh,
                u,
                v,
                u2,
                v2,
            },
        )]);
    }
}

#[derive(Copy, Clone)]
struct Sprite {
    x: f32,
    y: f32,
    hw: f32,
    hh: f32,
    u: f32,
    v: f32,
    u2: f32,
    v2: f32,
}

impl VertexCommand for Sprite {
    type Vertex = SpriteVertex;

    #[inline]
    fn draw(&self, queuer: &mut impl VertexQueuer<Vertex = Self::Vertex>) {
        let Sprite {
            x,
            y,
            hw,
            hh,
            u,
            v,
            u2,
            v2,
        } = *self;

        queuer.vertices([
            SpriteVertex::new(x - hw, y - hh, u, v),
            SpriteVertex::new(x + hw, y - hh, u2, v),
            SpriteVertex::new(x + hw, y + hh, u2, v2),
            SpriteVertex::new(x - hw, y + hh, u, v2),
        ]);

        queuer.indices([0, 1, 2, 2, 3, 0]);
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(ImagePlugin::default_nearest()))
        .add_plugins(HephaePlugin::<SpriteVertex>::new())
        .add_plugins(DrawerPlugin::<DrawSprite>::new())
        .add_systems(Startup, startup)
        .run();
}

fn startup(mut commands: Commands, server: Res<AssetServer>) {
    commands.spawn((
        Camera2dBundle {
            camera: Camera { hdr: true, ..default() },
            ..Camera2dBundle::new_with_far(1000.0)
        },
        BloomSettings::NATURAL,
    ));

    for translation in [
        Vec3::new(-200.0, -200.0, 0.0),
        Vec3::new(200.0, -200.0, 0.0),
        Vec3::new(200.0, 200.0, 0.0),
        Vec3::new(-200.0, 200.0, 0.0),
    ] {
        commands.spawn((
            TransformBundle {
                local: Transform {
                    translation,
                    scale: Vec3::splat(10.0),
                    ..default()
                },
                ..default()
            },
            VisibilityBundle::default(),
            server.load::<TextureAtlas>("sprites/sprites.atlas"),
            AtlasEntry("cix".into()),
            AtlasIndex::default(),
            HasDrawer::<DrawSprite>::new(),
        ));
    }
}
