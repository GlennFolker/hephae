use bevy::{
    core_pipeline::bloom::BloomSettings,
    ecs::{
        query::QueryItem,
        system::{lifetimeless::SRes, SystemParamItem},
    },
    prelude::*,
    render::render_resource::{BufferAddress, RenderPipelineDescriptor, VertexAttribute, VertexFormat},
};
use hephae::prelude::*;

#[derive(Clone, Copy, Pod, Zeroable)]
#[repr(C)]
struct Vert {
    pos: [f32; 2],
    color: LinearRgba,
}

impl Vert {
    #[inline]
    const fn new(x: f32, y: f32, red: f32, green: f32, blue: f32, alpha: f32) -> Self {
        Self {
            pos: [x, y],
            color: LinearRgba { red, green, blue, alpha },
        }
    }
}

impl Vertex for Vert {
    type PipelineParam = ();
    type PipelineProp = ();
    type PipelineKey = ();

    type Command = Quad;

    type BatchParam = ();
    type BatchProp = ();

    type RenderCommand = ();

    const SHADER: &'static str = "quad.wgsl";
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

    #[inline]
    fn setup(_: &mut App) {}

    #[inline]
    fn init_pipeline(_: SystemParamItem<Self::PipelineParam>) -> Self::PipelineProp {}

    #[inline]
    fn specialize_pipeline(_: Self::PipelineKey, _: &Self::PipelineProp, _: &mut RenderPipelineDescriptor) {}

    #[inline]
    fn create_batch(_: &mut SystemParamItem<Self::BatchParam>, _: Self::PipelineKey) -> Self::BatchProp {}
}

#[derive(Component, Copy, Clone)]
struct Draw;
impl Drawer for Draw {
    type ExtractParam = ();
    type DrawParam = SRes<Time>;
    type ExtractData = ();
    type ExtractFilter = ();

    type Vertex = Vert;

    #[inline]
    fn extract(_: &SystemParamItem<Self::ExtractParam>, _: QueryItem<Self::ExtractData>) -> Option<Self> {
        Some(Self)
    }

    #[inline]
    fn enqueue(
        &self,
        time: &SystemParamItem<Self::DrawParam>,
        queuer: &mut impl Extend<(f32, <Self::Vertex as Vertex>::PipelineKey, <Self::Vertex as Vertex>::Command)>,
    ) {
        queuer.extend([(0.0, (), Quad(time.elapsed_seconds()))]);
    }
}

#[derive(Copy, Clone)]
struct Quad(f32);
impl VertexCommand for Quad {
    type Vertex = Vert;

    #[inline]
    fn draw(&self, queuer: &mut impl VertexQueuer<Vertex = Self::Vertex>) {
        let (sin, cos) = (self.0 * 3.0).sin_cos();
        queuer.vertices([
            Vert::new(100.0 + cos * 25.0, 100.0 + sin * 25.0, 2.0, 0.0, 0.0, 1.0),
            Vert::new(-100.0 - cos * 25.0, 100.0 + sin * 25.0, 0.0, 3.0, 0.0, 1.0),
            Vert::new(-100.0 - cos * 25.0, -100.0 - sin * 25.0, 0.0, 0.0, 4.0, 1.0),
            Vert::new(100.0 + cos * 25.0, -100.0 - sin * 25.0, 4.0, 3.0, 2.0, 1.0),
        ]);

        queuer.indices([0, 1, 2, 2, 3, 0]);
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins(HephaePlugin::<Vert>::new())
        .add_plugins(DrawerPlugin::<Draw>::new())
        .add_systems(Startup, startup)
        .run();
}

fn startup(mut commands: Commands) {
    commands.spawn((
        Camera2dBundle {
            camera: Camera { hdr: true, ..default() },
            ..Camera2dBundle::new_with_far(1000.0)
        },
        BloomSettings::NATURAL,
    ));

    commands.spawn((
        VisibilityBundle::default(),
        TransformBundle::IDENTITY,
        HasDrawer::<Draw>::new(),
    ));
}
