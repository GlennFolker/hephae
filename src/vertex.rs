//! The heart of Hephae.
//!
//! See the documentation of [Vertex] for more information.

use std::{any::TypeId, hash::Hash, marker::PhantomData, sync::PoisonError};

use bevy::{
    core_pipeline::core_2d::Transparent2d,
    ecs::{
        component::ComponentId,
        entity::EntityHashMap,
        query::{QueryFilter, QueryItem, ReadOnlyQueryData},
        storage::SparseSet,
        system::{lifetimeless::Read, ReadOnlySystemParam, StaticSystemParam, SystemParam, SystemParamItem, SystemState},
        world::FilteredEntityRef,
    },
    prelude::*,
    render::{
        primitives::{Aabb, Frustum, Sphere},
        render_phase::RenderCommand,
        render_resource::{RenderPipelineDescriptor, VertexAttribute},
        view::{
            ExtractedView, NoCpuCulling, NoFrustumCulling, RenderLayers, VisibilityRange, VisibleEntities,
            VisibleEntityRanges,
        },
        Extract, Render, RenderApp,
    },
    utils::{Parallel, TypeIdMap},
};
use bytemuck::NoUninit;
use fixedbitset::FixedBitSet;

use crate::{pipeline::VertexQueues, HephaeSystems};

/// The heart of Hephae. Instances of `Vertex` directly represent the elements of the vertex buffer
/// in the GPU.
pub trait Vertex: Send + Sync + NoUninit {
    /// System parameter to fetch when initializing
    /// [`HephaePipeline`](crate::pipeline::HephaePipeline) to create a
    /// [`PipelineProp`](Vertex::PipelineProp).
    type PipelineParam: SystemParam;
    /// The additional property of the [common pipeline definition](crate::pipeline::HephaePipeline)
    /// that may used when specializing based on [`PipelineKey`](Vertex::PipelineKey). For example,
    /// this may be used to create a
    /// [`BindGroupLayout`](bevy::render::render_resource::BindGroupLayout) for texture-sampling.
    type PipelineProp: Send + Sync;
    /// Key used to specialize the render pipeline. For example, this may be an [`AssetId<Image>`]
    /// used to reference a [`GpuImage`](bevy::render::texture::GpuImage) for texture-sampling.
    type PipelineKey: Send + Sync + Clone + Eq + PartialEq + Hash;

    /// The vertex command that [`Drawer<Vertex = Self>`] may output. These commands will be sorted
    /// according to their Z-layers and then [extracted out](VertexCommand::draw) into the batches.
    type Command: VertexCommand<Vertex = Self>;

    /// System parameter to fetch when [creating the batch](Vertex::create_batch).
    type BatchParam: SystemParam;
    /// Additional property that is embedded into [`HephaeBatch`](crate::pipeline::HephaeBatch)
    /// components for use in [`RenderCommand`](Vertex::RenderCommand). For example, this may be an
    /// [`AssetId<Image>`] from [`PipelineKey`](Vertex::PipelineKey) to attach the associated bind
    /// group for texture-sampling.
    type BatchProp: Send + Sync;

    /// Additional GPU render commands to invoke before actually drawing the vertex and index
    /// buffers. For example, this may be used to set the texture-sampling bind group provided by
    /// [`BatchProp`](Vertex::BatchProp).
    type RenderCommand: RenderCommand<Transparent2d> + Send + Sync;

    /// Path to the shader rendering vertex attributes of this type. Entry points should be
    /// `vertex(...)` and `fragment(...)`.
    const SHADER: &'static str;
    /// Vertex attribute layout of this type. Ideally should match the fields `impl`ementors
    /// declare.
    const LAYOUT: &'static [VertexAttribute];

    /// Further customizes the application. Called in [`Plugin::finish`]. For example, this may be
    /// used to add systems extracting texture atlas pages and validating bind groups associated
    /// with them.
    #[allow(unused)]
    fn setup(app: &mut App) {}

    /// Creates the additional render pipeline property for use in
    /// [specialization](Vertex::specialize_pipeline).
    fn init_pipeline(param: SystemParamItem<Self::PipelineParam>) -> Self::PipelineProp;

    /// Specializes the render pipeline descriptor based off of the [key](Vertex::PipelineKey) and
    /// [prop](Vertex::PipelineProp) of the common render pipeline descriptor.
    fn specialize_pipeline(key: Self::PipelineKey, prop: &Self::PipelineProp, desc: &mut RenderPipelineDescriptor);

    /// Creates additional batch property for use in rendering.
    fn create_batch(param: &mut SystemParamItem<Self::BatchParam>, key: Self::PipelineKey) -> Self::BatchProp;
}

/// Commands issued by [Drawer]s that will flush out vertices and indices into buffers later.
pub trait VertexCommand: Send + Sync {
    /// The type of vertex this command works with.
    type Vertex: Vertex;

    /// Push vertices and indices to be rendered. For example, drawing a triangle would be calling
    /// `vertices([A, B, C])` and `indices([0, 1, 2])`.
    fn draw(&self, queuer: &mut impl VertexQueuer<Vertex = Self::Vertex>);
}

/// Similar to [`Extend`], except it works with both vertex and index buffers.
///
/// Ideally it also adjusts the index offset to the length of the current vertex buffer so
/// primitives would have the correct shapes.
pub trait VertexQueuer {
    /// The type of vertex this queuer works with.
    type Vertex: Vertex;

    /// Extends the vertex buffer with the supplied iterator.
    fn vertices(&mut self, vertices: impl IntoIterator<Item = Self::Vertex>);

    /// Extends the index buffer with the supplied iterator.
    fn indices(&mut self, indices: impl IntoIterator<Item = u32>);
}

/// Stores the runtime-only type information of [`Drawer`] that is associated with a [`Vertex`] for
/// use in [`check_visibilities`].
#[derive(Resource)]
pub struct VertexDrawers<T: Vertex>(SparseSet<ComponentId, TypeId>, PhantomData<fn() -> T>);
impl<T: Vertex> Default for VertexDrawers<T> {
    #[inline]
    fn default() -> Self {
        Self(SparseSet::new(), PhantomData)
    }
}

impl<T: Vertex> VertexDrawers<T> {
    /// Registers a [`Drawer`] to be checked in [`check_visibilities`].
    #[inline]
    pub fn add<D: Drawer<Vertex = T>>(&mut self, world: &mut World) {
        self.0
            .insert(world.init_component::<HasDrawer<D>>(), TypeId::of::<With<HasDrawer<D>>>());
    }
}

/// Calculates [`ViewVisibility`] of [drawable](Drawer) entities.
/// 
/// Similar to [`check_visibility`](bevy::render::view::check_visibility) that is generic over
/// [`HasDrawer`], except the filters are configured dynamically by [`DrawerPlugin`]. This makes it
/// so that all drawers that share the same [`Vertex`] type also share the same visibility system.
pub fn check_visibilities<T: Vertex>(
    world: &mut World,
    mut views: Local<
        SystemState<(
            Query<(
                Entity,
                Read<Frustum>,
                Option<Read<RenderLayers>>,
                Read<Camera>,
                Has<NoCpuCulling>,
            )>,
            Query<(
                Entity,
                &InheritedVisibility,
                &mut ViewVisibility,
                Option<&RenderLayers>,
                Option<&Aabb>,
                &GlobalTransform,
                Has<NoFrustumCulling>,
                Has<VisibilityRange>,
            )>,
            Option<Res<VisibleEntityRanges>>,
        )>,
    >,
    mut visible_entities: Local<SystemState<Query<(Entity, &mut VisibleEntities)>>>,
    mut visibility: Local<QueryState<FilteredEntityRef>>,
    mut thread_queues: Local<Parallel<Vec<Entity>>>,
    mut view_queues: Local<EntityHashMap<Vec<Entity>>>,
    mut view_maps: Local<EntityHashMap<TypeIdMap<Vec<Entity>>>>,
) {
    let drawers = world.resource_ref::<VertexDrawers<T>>();
    if drawers.is_changed() {
        let indices = drawers.0.indices().collect::<Box<_>>();
        let mut builder = QueryBuilder::<FilteredEntityRef>::new(world);

        builder.or(|query| {
            for &id in &indices {
                query.with_id(id);
            }
        });

        *visibility = builder.build();
    }

    let (view_query, mut visible_aabb_query, visible_entity_ranges) = views.get_mut(world);

    let visible_entity_ranges = visible_entity_ranges.as_deref();
    for (view, &frustum, maybe_view_mask, camera, no_cpu_culling) in &view_query {
        if !camera.is_active {
            continue
        }

        let view_mask = maybe_view_mask.unwrap_or_default();
        visible_aabb_query.par_iter_mut().for_each_init(
            || thread_queues.borrow_local_mut(),
            |queue,
             (
                entity,
                inherited_visibility,
                mut view_visibility,
                maybe_entity_mask,
                maybe_model_aabb,
                transform,
                no_frustum_culling,
                has_visibility_range,
            )| {
                if !inherited_visibility.get() {
                    return
                }

                let entity_mask = maybe_entity_mask.unwrap_or_default();
                if !view_mask.intersects(entity_mask) {
                    return;
                }

                // If outside of the visibility range, cull.
                if has_visibility_range &&
                    visible_entity_ranges.is_some_and(|visible_entity_ranges| {
                        !visible_entity_ranges.entity_is_in_range_of_view(entity, view)
                    })
                {
                    return;
                }

                // If we have an AABB, do frustum culling.
                if !no_frustum_culling && !no_cpu_culling {
                    if let Some(model_aabb) = maybe_model_aabb {
                        let world_from_local = transform.affine();
                        let model_sphere = Sphere {
                            center: world_from_local.transform_point3a(model_aabb.center),
                            radius: transform.radius_vec3a(model_aabb.half_extents),
                        };

                        // Do quick sphere-based frustum culling.
                        if !frustum.intersects_sphere(&model_sphere, false) {
                            return;
                        }

                        // Do AABB-based frustum culling.
                        if !frustum.intersects_obb(model_aabb, &world_from_local, true, false) {
                            return;
                        }
                    }
                }

                view_visibility.set();
                queue.push(entity);
            },
        );

        thread_queues.drain_into(view_queues.entry(view).or_default());
    }

    let drawers = world.resource::<VertexDrawers<T>>();
    for (&view, queues) in &mut view_queues {
        let map = view_maps.entry(view).or_default();
        for e in queues.drain(..) {
            let Ok(visible) = visibility.get(world, e) else { continue };
            for (&id, &key) in drawers.0.iter() {
                if visible.contains_id(id) {
                    map.entry(key).or_default().push(e);
                }
            }
        }
    }

    let mut visible_entities = visible_entities.get_mut(world);
    for (view, mut visible_entities) in &mut visible_entities {
        let Some(map) = view_maps.get_mut(&view) else { continue };
        for (&id, entities) in map {
            let dst = visible_entities.entities.entry(id).or_default();
            dst.append(entities);
        }
    }
}

/// Integrates [`Drawer`] into your application for entities to render into the Hephae rendering
/// pipeline.
pub struct DrawerPlugin<T: Drawer>(pub PhantomData<fn() -> T>);
impl<T: Drawer> Default for DrawerPlugin<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Drawer> DrawerPlugin<T> {
    /// Shortcut for `DrawerPlugin(PhantomData)`.
    #[inline]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T: Drawer> Plugin for DrawerPlugin<T> {
    fn build(&self, app: &mut App) {
        app.world_mut()
            .resource_scope::<VertexDrawers<T::Vertex>, ()>(|world, mut drawers| drawers.add::<T>(world));

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_systems(ExtractSchedule, extract_drawers::<T>)
                .add_systems(Render, queue_drawers::<T>.in_set(HephaeSystems::QueueDrawers));
        }
    }
}

/// A render world [`Component`] extracted from the main world that will be used to issue
/// [`VertexCommand`]s.
pub trait Drawer: Component + Sized {
    /// The type of vertex this drawer works with.
    type Vertex: Vertex;

    /// System parameter to fetch when extracting data from the main world.
    type ExtractParam: ReadOnlySystemParam;
    /// Query item to fetch from entities when extracting from those entities to the render world.
    type ExtractData: ReadOnlyQueryData;
    /// Additional query filters accompanying [`ExtractData`](Drawer::ExtractData).
    type ExtractFilter: QueryFilter;

    /// System parameter to fetch when issuing [`VertexCommand`]s.
    type DrawParam: ReadOnlySystemParam;

    /// Extracts an instance of this drawer from matching entities, if available.
    fn extract(param: &SystemParamItem<Self::ExtractParam>, query: QueryItem<Self::ExtractData>) -> Option<Self>;

    /// Issues [`VertexCommand`]s for rendering, in a form of Z-layer, [pipeline
    /// key](Vertex::PipelineKey), and [vertex command](Vertex::Command).
    fn enqueue(
        &self,
        param: &SystemParamItem<Self::DrawParam>,
        queuer: &mut impl Extend<(f32, <Self::Vertex as Vertex>::PipelineKey, <Self::Vertex as Vertex>::Command)>,
    );
}

/// Marker component for entities that may extract out [`Drawer`]s to the render world. This *must*
/// be added to those entities so they'll be calculated in [`check_visibilities`].
#[derive(Component, Copy, Clone)]
pub struct HasDrawer<T: Drawer>(pub PhantomData<fn() -> T>);
impl<T: Drawer> Default for HasDrawer<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Drawer> HasDrawer<T> {
    /// Shortcut for `HasDrawer(PhantomData)`.
    #[inline]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

/// Extracts an instance of `T` from matching entities.
pub fn extract_drawers<T: Drawer>(
    mut commands: Commands,
    param: Extract<T::ExtractParam>,
    query: Extract<Query<(Entity, &ViewVisibility, T::ExtractData), T::ExtractFilter>>,
    mut last_len: Local<usize>,
) {
    let mut values = Vec::with_capacity(*last_len);
    for (e, &view, data) in &query {
        if view.get() {
            if let Some(drawer) = T::extract(&param, data) {
                values.push((e, drawer));
            }
        }
    }

    *last_len = values.len();
    commands.insert_or_spawn_batch(values);
}

/// Collects [`VertexCommand`]s from drawers to be sorted by the pipeline.
pub fn queue_drawers<T: Drawer>(
    param: StaticSystemParam<T::DrawParam>,
    query: Query<&T>,
    views: Query<(Entity, &VisibleEntities), With<ExtractedView>>,
    queues: Res<VertexQueues<T::Vertex>>,
    mut iterated: Local<FixedBitSet>,
) {
    iterated.clear();
    for (view_entity, visible_entities) in &views {
        for &e in visible_entities.iter::<With<HasDrawer<T>>>() {
            let index = e.index() as usize;
            if iterated[index] {
                continue;
            }

            let Ok(drawer) = query.get(e) else { continue };

            iterated.grow_and_insert(index);
            queues.entities.entry(view_entity).or_default().insert(e);

            drawer.enqueue(&param, &mut *queues.commands.entry(e).or_default());
        }
    }

    queues
        .entity_bits
        .write()
        .unwrap_or_else(PoisonError::into_inner)
        .union_with(&iterated);
}
