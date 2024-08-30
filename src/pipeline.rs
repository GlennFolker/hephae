use std::marker::PhantomData;

use bevy::{
    core_pipeline::tonemapping::{get_lut_bind_group_layout_entries, Tonemapping},
    ecs::system::SystemState,
    prelude::*,
    render::{
        mesh::PrimitiveTopology,
        render_resource::{
            binding_types::{sampler, texture_2d, uniform_buffer},
            BindGroupLayout, BlendState, BufferAddress, ColorTargetState, ColorWrites, FragmentState, FrontFace,
            MultisampleState, PolygonMode, PrimitiveState, RenderPipelineDescriptor, SamplerBindingType, ShaderDefVal,
            ShaderStages, SpecializedRenderPipeline, TextureFormat, TextureSampleType, VertexBufferLayout, VertexState,
            VertexStepMode,
        },
        renderer::RenderDevice,
        texture::BevyDefault,
        view::{ViewTarget, ViewUniform},
    },
};

use crate::vertex::{Vertex, VertexKey};

#[derive(Resource)]
pub struct HephaePipeline<T: Vertex> {
    view_layout: BindGroupLayout,
    material_layout: BindGroupLayout,
    _marker: PhantomData<fn() -> T>,
}

impl<T: Vertex> FromWorld for HephaePipeline<T> {
    fn from_world(world: &mut World) -> Self {
        let device = SystemState::<Res<RenderDevice>>::new(world).get_mut(world);
        let [lut_texture, lut_sampler] = get_lut_bind_group_layout_entries();
        let view_layout = device.create_bind_group_layout("hephae_view_layout", &[
            uniform_buffer::<ViewUniform>(true).build(0, ShaderStages::VERTEX),
            lut_texture.build(1, ShaderStages::FRAGMENT),
            lut_sampler.build(2, ShaderStages::FRAGMENT),
        ]);

        let material_layout = device.create_bind_group_layout("hephae_material_layout", &[
            texture_2d(TextureSampleType::Float { filterable: true }).build(0, ShaderStages::FRAGMENT),
            sampler(SamplerBindingType::Filtering).build(1, ShaderStages::FRAGMENT),
        ]);

        Self {
            view_layout,
            material_layout,
            _marker: PhantomData,
        }
    }
}

impl<T: Vertex> SpecializedRenderPipeline for HephaePipeline<T> {
    type Key = (HephaeKey, T::Key);

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        let (common, key) = key;
        let mut defs = Vec::new();
        if let Some(tonemapping) = common.tonemapping {
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

            if common.deband_dither {
                defs.push("DEBAND_DITHER".into());
            }
        }

        let format = match common.hdr {
            true => ViewTarget::TEXTURE_FORMAT_HDR,
            false => TextureFormat::bevy_default(),
        };

        let mut desc = RenderPipelineDescriptor {
            label: Some("hephae_pipeline_descriptor".into()),
            layout: [self.view_layout.clone(), self.material_layout.clone()].into(),
            push_constant_ranges: Vec::new(),
            vertex: VertexState {
                shader: T::SHADER,
                shader_defs: defs.clone(),
                entry_point: "vertex_main".into(),
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
                count: 1 << common.msaa,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            fragment: Some(FragmentState {
                shader: T::SHADER,
                shader_defs: defs,
                entry_point: "fragment_main".into(),
                targets: [Some(ColorTargetState {
                    format,
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::ALL,
                })]
                .into(),
            }),
        };

        key.specialize(&mut desc);
        desc
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct HephaeKey {
    pub hdr: bool,
    pub msaa: u8,
    pub tonemapping: Option<Tonemapping>,
    pub deband_dither: bool,
}
