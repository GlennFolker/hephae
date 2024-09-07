pub mod atlas;
pub mod pipeline;
pub mod vertex;

use std::{marker::PhantomData, path::Path};

use atlas::{TextureAtlas, TextureAtlasLoader};
use bevy::{
    core_pipeline::core_2d::Transparent2d,
    ecs::system::ReadOnlySystemParam,
    prelude::*,
    render::{
        render_phase::{AddRenderCommand, RenderCommand},
        render_resource::SpecializedRenderPipelines,
        Render, RenderApp, RenderSet,
    },
};
use pipeline::{
    extract_shader, load_shader, prepare_batch, prepare_view_bind_groups, queue_batches, queue_vertices, DrawRequests,
    HephaeBatches, HephaePipeline, VertexQueues,
};
use vertex::Vertex;

pub mod prelude {
    pub use ::bytemuck::{self, NoUninit, Pod, Zeroable};

    pub use crate::{
        atlas::TextureAtlas,
        vertex::{Drawer, DrawerPlugin, HasDrawer, Vertex, VertexCommand, VertexKey, VertexQueuer},
        HephaePlugin,
    };
}

pub const HEPHAE_VIEW_BINDINGS_HANDLE: Handle<Shader> = Handle::weak_from_u128(278527494526026980866063021704582553601);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, SystemSet)]
pub enum HephaeSystems {
    ExtractDrawers,
    QueueBatches,
    QueueDrawers,
    QueueVertices,
    PrepareBatches,
    PrepareBindGroups,
}

pub struct HephaePlugin<T: Vertex>(PhantomData<fn() -> T>);
impl<T: Vertex> Default for HephaePlugin<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Vertex> HephaePlugin<T> {
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
        let mut assets = app.world_mut().resource_mut::<Assets<Shader>>();
        if !assets.contains(&HEPHAE_VIEW_BINDINGS_HANDLE) {
            assets.insert(
                &HEPHAE_VIEW_BINDINGS_HANDLE,
                Shader::from_wgsl(
                    include_str!("hephae_view_bindings.wgsl"),
                    Path::new(file!())
                        .parent()
                        .unwrap()
                        .join("hephae_view_bindings.wgsl")
                        .to_string_lossy(),
                ),
            );
        }

        app.init_asset::<TextureAtlas>()
            .register_asset_reflect::<TextureAtlas>()
            .register_asset_loader(TextureAtlasLoader)
            .add_systems(Startup, load_shader::<T>);

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .init_resource::<SpecializedRenderPipelines<HephaePipeline<T>>>()
                .init_resource::<VertexQueues<T>>()
                .init_resource::<HephaeBatches<T>>()
                .add_render_command::<Transparent2d, DrawRequests<T>>()
                .configure_sets(
                    Render,
                    (
                        (
                            HephaeSystems::QueueBatches,
                            HephaeSystems::QueueDrawers,
                            HephaeSystems::QueueVertices,
                        )
                            .in_set(RenderSet::Queue),
                        (HephaeSystems::QueueDrawers, HephaeSystems::QueueVertices).chain(),
                        HephaeSystems::PrepareBatches.in_set(RenderSet::Prepare),
                        HephaeSystems::PrepareBindGroups.in_set(RenderSet::PrepareBindGroups),
                    ),
                )
                .add_systems(ExtractSchedule, extract_shader::<T>)
                .add_systems(
                    Render,
                    (
                        queue_batches::<T>.in_set(HephaeSystems::QueueBatches),
                        queue_vertices::<T>.in_set(HephaeSystems::QueueVertices),
                        prepare_batch::<T>.in_set(HephaeSystems::PrepareBatches),
                        prepare_view_bind_groups::<T>.in_set(HephaeSystems::PrepareBindGroups),
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
