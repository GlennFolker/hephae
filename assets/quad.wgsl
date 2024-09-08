#ifdef TONEMAP_IN_SHADER
#import bevy_core_pipeline::tonemapping
#endif

#import hephae::view_bindings::view

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
}

@vertex
fn vertex(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = view.clip_from_world * vec4<f32>(in.position, 0.0, 1.0);
    out.color = in.color;

    return out;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    var color = in.color;

    #ifdef TONEMAP_IN_SHADER
    color = tonemapping::tone_mapping(color, view.color_grading);
    #endif

    return color;
}
