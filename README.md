# Hephae

> A personalized, opinionated Bevy plugin that adds support for drawing and batching arbitrary vertices and indices.

I initially wrote this plugin as a personal measure of generalizing and expanding `bevy_sprite` into supporting rendering arbitrary vertex attributes and indices that define such triangles, along with customizing pipelines and render commands down to their very cores. Expanding on "personal measure," the library may feel complicated and inconcise, as it is meant to be the set of puzzle blocks your utilize to define your own 2D rendering framework. Documentation is also fairly scarce and may not fully document caveats of the library; use at your own risk!

## Usage

In short, this library provides the following for you to build your framework on:

- `Vertex`: The heart of Hephae. Defines the vertex buffer layout, rendering pipeline specialization, batching parameters, and draw commands.
- `Drawer`: A render-world `Component` extracted from entities with `HasDrawer<T>`, acting as the "commander" to push out vertices and indices according to their logic-world entity parameters.
- `VertexCommand`: A "draw command" issued by `Drawer`, cached and sorted in the pipeline and modifies the GPU buffers directly when dispatched by camera views.
- `VertexPlugin<T: Vertex>`: Attaches Hephae vertex systems generic over `T` to the application.
- `DrawerPlugin<T: Drawer>`: Attaches Hephae vertex-drawer systems generic over `T` to the application.

The five of these are enough to build a spriteless colorful 2D rendering system (see `examples/quad.rs`). Please refer to the item-level documentations for more in-depth explanations and usage guides.

## License

All code in this repository is dual-licensed under either:

- MIT License ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)

at your option.
