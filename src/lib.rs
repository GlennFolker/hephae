#![doc = include_str!("../README.md")]
#![cfg_attr(doc, warn(missing_docs))]
#![allow(clippy::type_complexity)]
#![allow(clippy::too_many_arguments)]

#[cfg(feature = "atlas")]
pub mod atlas;
pub mod pipeline;
pub mod vertex;

use std::marker::PhantomData;

use bevy::{
    core_pipeline::core_2d::Transparent2d,
    ecs::system::ReadOnlySystemParam,
    prelude::*,
    render::{
        render_phase::{AddRenderCommand, RenderCommand},
        render_resource::SpecializedRenderPipelines,
        view::VisibilitySystems,
        Render, RenderApp, RenderSet,
    },
};
use pipeline::{
    clear_batches, extract_shader, load_shader, prepare_batch, prepare_view_bind_groups, queue_vertices, DrawRequests,
    HephaeBatches, HephaePipeline, VertexQueues,
};
use vertex::{check_visibilities, Vertex, VertexDrawers};

/// Prelude module containing commonly imported items.
pub mod prelude {
    pub use ::bytemuck::{self, NoUninit, Pod, Zeroable};

    pub use crate::{
        vertex::{Drawer, DrawerPlugin, Vertex, VertexCommand, VertexQueuer},
        HephaePlugin, HephaeSystems,
    };
}

/// Global handle to the global shader containing bind groups defining view uniform and tonemapping
/// LUTs.
pub const HEPHAE_VIEW_BINDINGS_HANDLE: Handle<Shader> = Handle::weak_from_u128(278527494526026980866063021704582553601);

/// Labels assigned to Hephae systems that are added to [`Render`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, SystemSet)]
pub enum HephaeSystems {
    /// Label for [`clear_batches`], in [`RenderSet::Queue`].
    ClearBatches,
    /// Label for [`queue_drawers`](vertex::queue_drawers), in
    /// [`RenderSet::Queue`].
    QueueDrawers,
    /// Label for [`queue_vertices`], in [`RenderSet::Queue`].
    QueueVertices,
    /// Label for [`prepare_batch`] and [`prepare_view_bind_groups`], in
    /// [`RenderSet::PrepareBindGroups`].
    PrepareBindGroups,
}

/// The entry point of Hephae, generic over `T`.
/// 
/// Adds the core functionality of Hephae through the
/// [`Vertex`] `impl` of `T`. Note that with this alone you can't start drawing yet; refer to
/// [`DrawerPlugin`](vertex::DrawerPlugin) for more.
pub struct HephaePlugin<T: Vertex>(PhantomData<fn() -> T>);
impl<T: Vertex> Default for HephaePlugin<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Vertex> HephaePlugin<T> {
    /// Constructs the plugin for use in [`App::add_plugins`].
    #[inline]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T: Vertex> Plugin for HephaePlugin<T>
where
    <T::RenderCommand as RenderCommand<Transparent2d>>::Param: ReadOnlySystemParam,
{
    fn build(&self, app: &mut App) {
        #[derive(Resource)]
        struct Common;

        let run = !app.world().contains_resource::<Common>();
        if run {
            app.insert_resource(Common);
            app.world_mut().resource_mut::<Assets<Shader>>().insert(
                &HEPHAE_VIEW_BINDINGS_HANDLE,
                Shader::from_wgsl(include_str!("view_bindings.wgsl"), "hephae/view_bindings.wgsl"),
            );
        }

        app.init_resource::<VertexDrawers<T>>()
            .add_systems(Startup, load_shader::<T>)
            .add_systems(PostUpdate, check_visibilities::<T>.in_set(VisibilitySystems::CheckVisibility));

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            if run {
                render_app.configure_sets(
                    Render,
                    (
                        (
                            HephaeSystems::ClearBatches,
                            HephaeSystems::QueueDrawers,
                            HephaeSystems::QueueVertices,
                        )
                            .in_set(RenderSet::Queue),
                        HephaeSystems::QueueDrawers.before_ignore_deferred(HephaeSystems::QueueVertices),
                        HephaeSystems::PrepareBindGroups.in_set(RenderSet::PrepareBindGroups),
                    ),
                );
            }

            render_app
                .init_resource::<SpecializedRenderPipelines<HephaePipeline<T>>>()
                .init_resource::<VertexQueues<T>>()
                .init_resource::<HephaeBatches<T>>()
                .add_render_command::<Transparent2d, DrawRequests<T>>()
                .add_systems(ExtractSchedule, extract_shader::<T>)
                .add_systems(
                    Render,
                    (
                        clear_batches::<T>.in_set(HephaeSystems::ClearBatches),
                        queue_vertices::<T>.in_set(HephaeSystems::QueueVertices),
                        (prepare_batch::<T>, prepare_view_bind_groups::<T>).in_set(HephaeSystems::PrepareBindGroups),
                    ),
                );
        }
    }

    fn finish(&self, app: &mut App) {
        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app.init_resource::<HephaePipeline<T>>();
        }

        T::setup(app);
    }
}
