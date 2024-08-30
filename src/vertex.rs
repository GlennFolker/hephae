use std::hash::Hash;

use bevy::{
    prelude::*,
    render::render_resource::{RenderPipelineDescriptor, VertexAttribute},
};
use bytemuck::NoUninit;

pub trait Vertex: Send + Sync + NoUninit {
    type Key: VertexKey;

    const SHADER: Handle<Shader>;
    const SHADER_SOURCE: &'static str;
    const LAYOUT: &'static [VertexAttribute];
}

pub trait VertexKey: Send + Sync + Clone + Eq + PartialEq + Hash {
    fn specialize(self, desc: &mut RenderPipelineDescriptor);
}
