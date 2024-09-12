use std::{hash::Hash, marker::PhantomData, sync::PoisonError};

use bevy::{
    core_pipeline::core_2d::Transparent2d,
    ecs::{
        query::{QueryFilter, QueryItem, ReadOnlyQueryData},
        system::{ReadOnlySystemParam, StaticSystemParam, SystemParam, SystemParamItem},
    },
    prelude::*,
    render::{
        render_phase::RenderCommand,
        render_resource::{RenderPipelineDescriptor, VertexAttribute},
        view::{ExtractedView, VisibleEntities},
        Extract, Render, RenderApp,
    },
    utils::EntityHashSet,
};
use bytemuck::NoUninit;
use fixedbitset::FixedBitSet;

use crate::{pipeline::VertexQueues, HephaeSystems};

/// The heart of Hephae. Instances of `Vertex` define
pub trait Vertex: Send + Sync + NoUninit {
    type PipelineParam: SystemParam;
    type PipelineProp: Send + Sync;
    type PipelineKey: Send + Sync + Clone + Eq + PartialEq + Hash;

    type Command: VertexCommand<Vertex = Self>;

    type BatchParam: SystemParam;
    type BatchProp: Send + Sync;

    type RenderCommand: RenderCommand<Transparent2d> + Send + Sync;

    const SHADER: &'static str;
    const LAYOUT: &'static [VertexAttribute];

    #[allow(unused)]
    fn setup(app: &mut App) {}

    fn init_pipeline(param: SystemParamItem<Self::PipelineParam>) -> Self::PipelineProp;

    fn specialize_pipeline(key: Self::PipelineKey, prop: &Self::PipelineProp, desc: &mut RenderPipelineDescriptor);

    fn create_batch(param: &mut SystemParamItem<Self::BatchParam>, key: Self::PipelineKey) -> Self::BatchProp;
}

pub trait VertexCommand: Send + Sync {
    type Vertex: Vertex;

    fn draw(&self, queuer: &mut impl VertexQueuer<Vertex = Self::Vertex>);
}

pub trait VertexQueuer {
    type Vertex: Vertex;

    fn vertices(&mut self, vertices: impl IntoIterator<Item = Self::Vertex>);

    fn indices(&mut self, indices: impl IntoIterator<Item = u32>);
}

#[derive(Component, Copy, Clone)]
pub struct HasVertex<T: Vertex>(pub PhantomData<fn() -> T>);
impl<T: Vertex> Default for HasVertex<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Vertex> HasVertex<T> {
    #[inline]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

pub struct DrawerPlugin<T: Drawer>(pub PhantomData<fn() -> T>);
impl<T: Drawer> Default for DrawerPlugin<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Drawer> DrawerPlugin<T> {
    #[inline]
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T: Drawer> Plugin for DrawerPlugin<T> {
    fn build(&self, app: &mut App) {
        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_systems(ExtractSchedule, extract_drawers::<T>)
                .add_systems(Render, queue_drawers::<T>.in_set(HephaeSystems::QueueDrawers));
        }
    }
}

pub trait Drawer: Component + Sized {
    type Vertex: Vertex;

    type ExtractParam: ReadOnlySystemParam;
    type ExtractData: ReadOnlyQueryData;
    type ExtractFilter: QueryFilter;

    type DrawParam: ReadOnlySystemParam;

    fn extract(param: &SystemParamItem<Self::ExtractParam>, query: QueryItem<Self::ExtractData>) -> Option<Self>;

    fn enqueue(
        &self,
        param: &SystemParamItem<Self::DrawParam>,
        queuer: &mut impl Extend<(f32, <Self::Vertex as Vertex>::PipelineKey, <Self::Vertex as Vertex>::Command)>,
    );
}

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

pub fn queue_drawers<T: Drawer>(
    param: StaticSystemParam<T::DrawParam>,
    query: Query<&T>,
    views: Query<(Entity, &VisibleEntities), With<ExtractedView>>,
    queues: Res<VertexQueues<T::Vertex>>,
    mut iterated: Local<FixedBitSet>,
) {
    iterated.clear();
    for (view_entity, visible_entities) in &views {
        for &e in visible_entities.iter::<With<HasVertex<T::Vertex>>>() {
            let index = e.index() as usize;
            if iterated[index] {
                continue;
            }

            let Ok(drawer) = query.get(e) else { continue };

            iterated.grow_and_insert(index);
            queues
                .entities
                .entry(view_entity)
                .or_insert(EntityHashSet::default())
                .insert(e);

            drawer.enqueue(&param, &mut *queues.commands.entry(e).or_insert(Vec::new()));
        }
    }

    queues
        .entity_bits
        .write()
        .unwrap_or_else(PoisonError::into_inner)
        .union_with(&iterated);
}
