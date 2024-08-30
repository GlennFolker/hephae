pub mod atlas;
pub mod pipeline;
pub mod vertex;

use std::{
    marker::PhantomData,
    ptr::null_mut,
    sync::{
        atomic::{AtomicPtr, Ordering},
        Arc,
    },
};

use atlas::{TextureAtlas, TextureAtlasLoader};
use bevy::{
    asset::LoadState,
    prelude::*,
    render::{render_resource::SpecializedRenderPipelines, RenderApp},
};
pub use bytemuck;
use pipeline::HephaePipeline;
use vertex::Vertex;

pub mod prelude {
    pub use ::bytemuck::{self, NoUninit, Pod, Zeroable};

    pub use crate::{
        atlas::TextureAtlas,
        vertex::{Vertex, VertexKey},
        HephaePlugin,
    };
}

pub const HEPHAE_VIEW_BINDINGS_HANDLE: Handle<Shader> = Handle::weak_from_u128(278527494526026980866063021704582553601);

pub struct HephaePlugin<T: Vertex> {
    _marker: PhantomData<fn() -> T>,
}

impl<T: Vertex> Default for HephaePlugin<T> {
    #[inline]
    fn default() -> Self {
        Self { _marker: PhantomData }
    }
}

impl<T: Vertex> Plugin for HephaePlugin<T> {
    #[inline]
    fn build(&self, app: &mut App) {
        let mut assets = app.world_mut().resource_mut::<Assets<Shader>>();
        if !assets.contains(&HEPHAE_VIEW_BINDINGS_HANDLE) {
            assets.insert(
                &HEPHAE_VIEW_BINDINGS_HANDLE,
                Shader::from_wgsl(
                    "
#define_import_path hephae::hephae_view_bindings

#import bevy_render::view::View

@group(0) @binding(0) var<uniform> view: View;

@group(0) @binding(1) var dt_lut_texture: texture_3d<f32>;
@group(0) @binding(2) var dt_lut_sampler: sampler;
            ",
                    "hephae/hephae_view_bindings.wgsl",
                ),
            );
        }

        let shader = Arc::new(AtomicPtr::<Handle<Shader>>::new(null_mut()));
        let shader_clone = shader.clone();

        app.init_asset::<TextureAtlas>()
            .register_asset_reflect::<TextureAtlas>()
            .register_asset_loader(TextureAtlasLoader)
            .add_systems(Startup, move |server: Res<AssetServer>| {
                shader_clone.store(Box::into_raw(Box::new(server.load(T::SHADER_SOURCE))), Ordering::Release)
            })
            .add_systems(Last, move |server: Res<AssetServer>, mut shaders: ResMut<Assets<Shader>>| {
                let ptr = shader.swap(null_mut(), Ordering::AcqRel);
                if !ptr.is_null() {
                    let handle = unsafe { Box::from_raw(ptr) };
                    match server.load_state(&*handle) {
                        LoadState::NotLoaded => panic!("shader `{}` is not loaded", T::SHADER_SOURCE),
                        LoadState::Failed(e) => panic!("shader `{}` failed to load: {}", T::SHADER_SOURCE, e),
                        LoadState::Loading => shader.store(Box::into_raw(handle), Ordering::Release),
                        LoadState::Loaded => {
                            let shader = shaders.remove_untracked(&*handle).unwrap();
                            shaders.insert(&T::SHADER, shader);
                        }
                    }
                }
            });

        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app.init_resource::<SpecializedRenderPipelines<HephaePipeline<T>>>();

            /*render_app
            .init_resource::<SpecializedRenderPipelines<ShapePipeline<T>>>()
            .init_resource::<Requests<T>>()
            .init_resource::<Batch<T>>()
            .init_resource::<DrawLayer<T>>()
            .add_render_command::<Transparent2d, DrawShapes<T>>()
            .configure_sets(
                Render,
                (
                    (ShapeSystems::QueueShaper, ShapeSystems::QueueVertices).in_set(RenderSet::Queue),
                    ShapeSystems::QueueVertices.after_ignore_deferred(ShapeSystems::QueueShaper),
                    ShapeSystems::PrepareBatch.in_set(RenderSet::Prepare),
                    ShapeSystems::PrepareBindGroup.in_set(RenderSet::PrepareBindGroups),
                ),
            )
            .add_systems(
                Render,
                (
                    queue_vertices::<T>.in_set(ShapeSystems::QueueVertices),
                    prepare_vertices_batch::<T>.in_set(ShapeSystems::PrepareBatch),
                    prepare_vertices_bind_group::<T>.in_set(ShapeSystems::PrepareBindGroup),
                ),
            );*/
        }
    }
}
