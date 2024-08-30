use bevy::{
    prelude::*,
    render::render_resource::{BufferAddress, RenderPipelineDescriptor, VertexAttribute, VertexFormat},
};
use hephae::prelude::*;

#[derive(Clone, Copy, Pod, Zeroable)]
#[repr(C)]
pub struct Quad {
    pub pos: [f32; 2],
    pub color: [f32; 4],
}

impl Vertex for Quad {
    type Key = QuadKey;

    const SHADER: Handle<Shader> = Handle::weak_from_u128(224986892615170955547233766276757289041);
    const SHADER_SOURCE: &'static str = "quad.wgsl";

    const LAYOUT: &'static [VertexAttribute] = &[
        VertexAttribute {
            format: VertexFormat::Float32x2,
            offset: 0,
            shader_location: 0,
        },
        VertexAttribute {
            format: VertexFormat::Float32x4,
            offset: size_of::<[f32; 2]>() as BufferAddress,
            shader_location: 1,
        },
    ];
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct QuadKey;
impl VertexKey for QuadKey {
    fn specialize(self, _: &mut RenderPipelineDescriptor) {}
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(HephaePlugin::<Quad>::default())
        .run();
}
