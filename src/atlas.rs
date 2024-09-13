//! Provides texture atlas functionality.
//!
//! A texture atlas contains atlas pages, i.e. lists of textures packed into one large texture in
//! order to reduce the amount of bind groups necessary to hold the information passed to shaders.
//! This means integrating a texture atlas into [`Vertex`](crate::vertex::Vertex) rendering will
//! significantly increase batching potential, leading to fewer GPU render calls.
//!
//! This module provides the [`TextureAtlas`] type, which also derives [`Asset`] and has an
//! associated [asset loader](TextureAtlasLoader) with it. Refer to [`TextureAtlasFile`] for the
//! specific format of `.atlas` files.
//!
//! This module also provides [`AtlasEntry`] and [`AtlasIndex`] [components](Component); the former
//! being the atlas lookup key, and the latter being the cached sprite index. The dedicated
//! [`update_atlas_index`] system listens to changes/additions to texture atlas assets and updates
//! the [`AtlasIndex`] of entities accordingly.
//!
//! Ideally, you'd want to associate each atlas pages with a
//! [`BindGroup`](bevy::render::render_resource::BindGroup), define a texture and sampler layout in
//! the [specialized pipeline](crate::vertex::Vertex::PipelineProp), somehow store a reference
//! to this bind group into the [batch entities](crate::vertex::Vertex::create_batch), and finally
//! set the [render pass](bevy::render::render_phase::TrackedRenderPass)'s bind group to the atlas
//! page bind group accordingly with the layout you defined earlier.
//!
//! See the `examples/sprite.rs` for a full example.

use std::{borrow::Cow, io::Error as IoError};

use bevy::{
    asset::{
        io::Reader, AssetLoadError, AssetLoader, AssetPath, AsyncReadExt, LoadContext, ParseAssetPathError, ReflectAsset,
    },
    prelude::*,
    render::{
        render_asset::RenderAssetUsages,
        render_resource::{Extent3d, TextureDimension, TextureFormat},
        texture::TextureFormatPixelInfo,
    },
    utils::{HashMap, HashSet},
};
use guillotiere::{
    euclid::{Box2D, Size2D},
    size2, AllocId, AtlasAllocator, Change, ChangeList,
};
use ron::de::SpannedError;
use serde::{
    de::{Error as DeError, MapAccess, Visitor},
    ser::SerializeStruct,
    Deserialize, Deserializer, Serialize, Serializer,
};
use thiserror::Error;

/// Provides texture atlas functionality. Registers [`TextureAtlas`] and [`TextureAtlasLoader`].
pub struct AtlasPlugin;
impl Plugin for AtlasPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<TextureAtlas>()
            .register_asset_reflect::<TextureAtlas>()
            .register_asset_loader(TextureAtlasLoader)
            .add_systems(PostUpdate, update_atlas_index);
    }
}

/// A list of textures packed into one large texture. See the [module-level](crate::atlas)
/// documentation for more specific information on how to integrate this into your rendering
/// framework.
#[derive(Asset, Reflect, Debug, Clone)]
#[reflect(Asset)]
pub struct TextureAtlas {
    /// The list of pages contained in this atlas. Items may be modified, but growing or shrinking
    /// this vector is **discouraged**.
    pub pages: Vec<AtlasPage>,
    /// Mapping of sprite names to `(P, Q)` where `P` is the [page index](Self::pages) and `Q` is
    /// the [sprite index](AtlasPage::sprites). Only ever modify if you know what you're doing.
    pub sprite_map: HashMap<String, (usize, usize)>,
}

/// A page located in a [`TextureAtlas`]. Contains the handle to the page image, and rectangle
/// placements of each sprites.
#[derive(Reflect, Debug, Clone)]
pub struct AtlasPage {
    /// The page handle.
    pub image: Handle<Image>,
    /// List of sprite rectangle placements in the page; may be looked up from
    /// [TextureAtlas::sprite_map].
    pub sprites: Vec<URect>,
}

/// Component denoting a texture atlas sprite lookup key. See the [module-level](crate::atlas)
/// documentation for more specific information on how to integrate this into your rendering
/// framework.
#[derive(Component, Debug, Clone, Deref, DerefMut)]
pub struct AtlasEntry(pub Cow<'static, str>);

/// Component denoting a texture atlas cached sprite index. See the [module-level](crate::atlas)
/// documentation for more specific information on how to integrate this into your rendering
/// framework.
#[derive(Component, Copy, Clone, Debug)]
pub struct AtlasIndex {
    page_index: usize,
    sprite_index: usize,
}

impl Default for AtlasIndex {
    #[inline]
    fn default() -> Self {
        Self {
            page_index: usize::MAX,
            sprite_index: usize::MAX,
        }
    }
}

impl AtlasIndex {
    /// Obtains the [page index](TextureAtlas::pages) and [sprite index](AtlasPage::sprites), or
    /// [`None`] if the [key](AtlasIndex) is invalid.
    #[inline]
    pub const fn indices(self) -> Option<(usize, usize)> {
        if self.page_index == usize::MAX || self.sprite_index == usize::MAX {
            None
        } else {
            Some((self.page_index, self.sprite_index))
        }
    }
}

/// System to update [`AtlasIndex`] according to changes [`AtlasEntry`] and [`TextureAtlas`] assets.
pub fn update_atlas_index(
    mut events: EventReader<AssetEvent<TextureAtlas>>,
    atlases: Res<Assets<TextureAtlas>>,
    mut entries: ParamSet<(
        Query<(&Handle<TextureAtlas>, &AtlasEntry, &mut AtlasIndex), Changed<AtlasEntry>>,
        Query<(&Handle<TextureAtlas>, &AtlasEntry, &mut AtlasIndex)>,
    )>,
    mut changed: Local<HashSet<AssetId<TextureAtlas>>>,
) {
    changed.clear();
    for &event in events.read() {
        if let AssetEvent::Added { id } | AssetEvent::Modified { id } = event {
            changed.insert(id);
        }
    }

    let update = |handle, entry, mut index: Mut<AtlasIndex>| {
        let Some(atlas) = atlases.get(handle) else { return };
        let Some(&(page_index, sprite_index)) = atlas.sprite_map.get(entry) else {
            *index = default();
            return;
        };

        *index = AtlasIndex {
            page_index,
            sprite_index,
        };
    };

    if changed.is_empty() {
        for (handle, entry, index) in &mut entries.p0() {
            update(handle, &***entry, index);
        }
    } else {
        for (handle, entry, mut index) in &mut entries.p1() {
            if !changed.contains(&handle.id()) {
                *index = default();
                continue;
            }

            update(handle, &***entry, index);
        }
    }
}

/// Asset file representation of [`TextureAtlas`].
///
/// This struct `impl`s [`Serialize`] and
/// [`Deserialize`], which means it may be (de)serialized into any implementation, albeit
/// [`TextureAtlasLoader`] uses [RON](ron) format specifically.
///
/// Format is as following:
/// ```ron
/// (
///     padding: 4,
///     bleeding: 4,
///     usages: (
///         main: false,
///         render: true,
///     ),
///     entries: [
///         "some-file-relative-to-atlas-file.png",
///         ("some-dir-relative-to-atlas-file", [
///             "some-file-inside-the-dir.png",
///         ]),
///     ],
/// )
/// ```
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TextureAtlasFile {
    /// How far away the edges of one sprite to another and to the page boundaries, in pixels. This
    /// may be utilized to mitigate the imperfect precision with texture sampling where a fraction
    /// of neighboring sprites actually get sampled instead.
    #[serde(default = "TextureAtlasFile::default_padding")]
    pub padding: u32,
    /// How much the sprites will "bleed" outside its edge. That is, how much times the edges of a
    /// sprite is copied to its surrounding border, creating a bleeding effect. This may be utilized
    /// to mitigate the imperfect precision with texture sampling where the edge of a sprite doesn't
    /// quite reach the edge of the vertices.
    #[serde(default = "TextureAtlasFile::default_bleeding")]
    pub bleeding: u32,
    #[serde(
        default = "TextureAtlasFile::default_usages",
        serialize_with = "TextureAtlasFile::serialize_usages",
        deserialize_with = "TextureAtlasFile::deserialize_usages"
    )]
    /// Defines the usages for the resulting atlas pages.
    pub usages: RenderAssetUsages,
    /// File entries relative to the atlas configuration file.
    pub entries: Vec<TextureAtlasEntry>,
}

impl TextureAtlasFile {
    /// Default padding of a texture atlas is 4 pixels.
    #[inline]
    pub const fn default_padding() -> u32 {
        4
    }

    /// Default bleeding of a texture atlas is 4 pixels.
    #[inline]
    pub const fn default_bleeding() -> u32 {
        4
    }

    /// Default usage of texture atlas pages is [RenderAssetUsages::RENDER_WORLD].
    #[inline]
    pub const fn default_usages() -> RenderAssetUsages {
        RenderAssetUsages::RENDER_WORLD
    }

    /// Serializes the usages into `(main: <bool>, render: <bool>)`.
    #[inline]
    pub fn serialize_usages<S: Serializer>(usages: &RenderAssetUsages, ser: S) -> Result<S::Ok, S::Error> {
        let mut u = ser.serialize_struct("RenderAssetUsages", 2)?;
        u.serialize_field("main", &usages.contains(RenderAssetUsages::MAIN_WORLD))?;
        u.serialize_field("render", &usages.contains(RenderAssetUsages::RENDER_WORLD))?;
        u.end()
    }

    /// Deserializes the usages from `(main: <bool>, render: <bool>)`.
    #[inline]
    pub fn deserialize_usages<'de, D: Deserializer<'de>>(de: D) -> Result<RenderAssetUsages, D::Error> {
        const FIELDS: &[&str] = &["main", "render"];

        struct Visit;
        impl<'de> Visitor<'de> for Visit {
            type Value = RenderAssetUsages;

            #[inline]
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "struct RenderAssetUsages {{ main: bool, render: bool }}")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut main = None::<bool>;
                let mut render = None::<bool>;
                while let Some(key) = map.next_key()? {
                    match key {
                        "main" => match main {
                            None => main = Some(map.next_value()?),
                            Some(..) => return Err(DeError::duplicate_field("main")),
                        },
                        "render" => match render {
                            None => render = Some(map.next_value()?),
                            Some(..) => return Err(DeError::duplicate_field("render")),
                        },
                        e => return Err(DeError::unknown_field(e, FIELDS)),
                    }
                }

                let main = main.ok_or(DeError::missing_field("main"))?;
                let render = render.ok_or(DeError::missing_field("render"))?;
                Ok(match (main, render) {
                    (false, false) => RenderAssetUsages::empty(),
                    (true, false) => RenderAssetUsages::MAIN_WORLD,
                    (false, true) => RenderAssetUsages::RENDER_WORLD,
                    (true, true) => RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
                })
            }
        }

        de.deserialize_struct("RenderAssetUsages", FIELDS, Visit)
    }
}

/// A [TextureAtlas] file entry. May either be a file or a directory containing files.
#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum TextureAtlasEntry {
    /// Defines a relative path to an [Image] file.
    File(String),
    /// Defines a directory relative to the current one, filled with more entries.
    Directory(String, Vec<TextureAtlasEntry>),
}

impl<T: ToString> From<T> for TextureAtlasEntry {
    #[inline]
    fn from(value: T) -> Self {
        Self::File(value.to_string())
    }
}

/// Additional settings that may be adjusted when loading a [TextureAtlas]. This is typically used
/// to limit texture sizes to what the rendering backend supports.
#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub struct TextureAtlasSettings {
    /// The initial width of an atlas page. Gradually grows to [Self::max_width] if insufficient.
    pub init_width: u32,
    /// The initial height of an atlas page. Gradually grows to [Self::max_height] if insufficient.
    pub init_height: u32,
    /// The maximum width of an atlas page. If insufficient, a new page must be allocated.
    pub max_width: u32,
    /// The maximum height of an atlas page. If insufficient, a new page must be allocated.
    pub max_height: u32,
}

impl Default for TextureAtlasSettings {
    #[inline]
    fn default() -> Self {
        Self {
            init_width: 1024,
            init_height: 1024,
            max_width: 2048,
            max_height: 2048,
        }
    }
}

/// Errors that may arise when loading a [`TextureAtlas`].
#[derive(Error, Debug)]
pub enum TextureAtlasError {
    /// Error that arises when a texture is larger than the maximum size of the atlas page.
    #[error("Texture '{name}' is too large: [{actual_width}, {actual_height}] > [{max_width}, {max_height}]")]
    TooLarge {
        /// The sprite lookup key.
        name: String,
        /// The maximum width of the atlas page. See [`TextureAtlasSettings::max_width`].
        max_width: u32,
        /// The maximum width of the atlas page. See [`TextureAtlasSettings::max_height`].
        max_height: u32,
        /// The width of the erroneous texture.
        actual_width: u32,
        /// The height of the erroneous texture.
        actual_height: u32,
    },
    /// Error that arises when the texture couldn't be converted into
    /// [`TextureFormat::Rgba8UnormSrgb`].
    #[error("Texture '{name}' has an unsupported format: {format:?}")]
    UnsupportedFormat {
        /// The sprite lookup key.
        name: String,
        /// The invalid texture format.
        format: TextureFormat,
    },
    /// Error that arises when the texture couldn't be loaded at all.
    #[error("Texture '{name}' failed to load: {error}")]
    InvalidImage {
        /// The sprite lookup key.
        name: String,
        /// The error that arises when trying to load the texture.
        error: AssetLoadError,
    },
    /// Error that arises when a texture has an invalid path string.
    #[error(transparent)]
    InvalidPath(#[from] ParseAssetPathError),
    /// Error that arises when the `.atlas` file has an invalid RON syntax.
    #[error(transparent)]
    InvalidFile(#[from] SpannedError),
    /// Error that arises when an IO error occurs.
    #[error(transparent)]
    Io(#[from] IoError),
}

/// Dedicated [`AssetLoader`] to load [`TextureAtlas`].
/// 
/// Parses file into [`TextureAtlasFile`]
/// representation, and accepts [`TextureAtlasSettings`] as additional optional configuration. May
/// throw [`TextureAtlasError`] for erroneous assets.
///
/// This asset loader adds each texture atlas entry as a "load dependency." As much, coupled with a
/// file system watcher, mutating these input image files will cause reprocessing of the atlas.
///
/// This asset loader also adds [`pages[i].image`](AtlasPage::image) as a labelled asset with label
/// `"page-{i}"` (without the brackets). Therefore, doing (for example)
/// `server.load::<Image>("sprites.atlas#page-0")` is possible and will return the 0th page image of
/// the atlas, provided the atlas actually has a 0th page.
pub struct TextureAtlasLoader;
impl AssetLoader for TextureAtlasLoader {
    type Asset = TextureAtlas;
    type Settings = TextureAtlasSettings;
    type Error = TextureAtlasError;

    async fn load<'a>(
        &'a self,
        reader: &'a mut Reader<'_>,
        settings: &'a Self::Settings,
        load_context: &'a mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let &Self::Settings {
            init_width,
            init_height,
            max_width,
            max_height,
        } = settings;

        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;

        let TextureAtlasFile {
            padding,
            bleeding,
            usages,
            entries: file_entries,
        } = ron::de::from_bytes(&bytes)?;

        drop(bytes);
        let padding = padding as usize;
        let bleeding = (bleeding as usize).min(padding);

        async fn collect(
            entry: TextureAtlasEntry,
            base: &AssetPath<'_>,
            load_context: &mut LoadContext<'_>,
            accum: &mut Vec<(String, Image)>,
        ) -> Result<(), TextureAtlasError> {
            match entry {
                TextureAtlasEntry::File(path) => {
                    let path = base.resolve(&path)?;
                    let Some(name) = path.path().file_stem() else { return Ok(()) };

                    let name = name.to_string_lossy().into_owned();
                    accum.push((
                        name.clone(),
                        load_context
                            .loader()
                            .direct()
                            .load::<Image>(&path)
                            .await
                            .map_err(|e| TextureAtlasError::InvalidImage { name, error: e.error })?
                            .take(),
                    ));
                }
                TextureAtlasEntry::Directory(dir, paths) => {
                    let base = base.resolve(&dir)?;
                    for path in paths {
                        Box::pin(collect(path, &base, load_context, accum)).await?;
                    }
                }
            }

            Ok(())
        }

        let mut entries = Vec::new();
        for file_entry in file_entries {
            collect(
                file_entry,
                &load_context.asset_path().parent().unwrap(),
                load_context,
                &mut entries,
            )
            .await?;
        }

        radsort::sort_by_key(&mut entries, |(.., texture)| {
            let UVec2 { x, y } = texture.size();
            2 * (x + y)
        });

        let mut atlas = TextureAtlas {
            pages: Vec::new(),
            sprite_map: HashMap::new(),
        };

        let mut end = |ids: HashMap<AllocId, (String, Image)>, packer: AtlasAllocator| {
            let Size2D {
                width: page_width,
                height: page_height,
                ..
            } = packer.size().to_u32();

            let size = TextureFormat::Rgba8UnormSrgb.pixel_size();
            let page_row_size = page_width as usize * size;
            let mut image = Image::new(
                Extent3d {
                    width: page_width,
                    height: page_height,
                    depth_or_array_layers: 1,
                },
                TextureDimension::D2,
                vec![0; page_row_size * page_height as usize],
                TextureFormat::Rgba8UnormSrgb,
                usages,
            );
            let mut sprites = Vec::new();

            for (id, (name, texture)) in ids {
                let Box2D { min, max } = packer[id].to_usize();
                let min_x = min.x + padding;
                let min_y = min.y + padding;
                let max_x = max.x - padding;
                let max_y = max.y - padding;

                let src_row_size = (max_x - min_x) * size;

                let Some(texture) = texture.convert(TextureFormat::Rgba8UnormSrgb) else {
                    return Err(TextureAtlasError::UnsupportedFormat {
                        name,
                        format: texture.texture_descriptor.format,
                    });
                };

                // Mem-set topleft-wards bleeding to topleft-most pixel and topright-wards
                // bleeding to topright-most pixel. This is so that the
                // subsequent bleeding operation may just use a split-off `copy_from_slice`.
                for bleed_x in 0..bleeding {
                    let left_x = (min_y - bleeding) * page_row_size + (min_x - bleed_x - 1) * size;
                    image.data[left_x..left_x + size].copy_from_slice(&texture.data[..size]);

                    let right_x = (min_y - bleeding) * page_row_size + (max_x + bleed_x) * size;
                    image.data[right_x..right_x + size].copy_from_slice(&texture.data[src_row_size - size..src_row_size]);
                }

                // Copy top-most edge to bleed upwards.
                image.data
                    [(min_y - bleeding) * page_row_size + min_x * size..(min_y - bleeding) * page_row_size + max_x * size]
                    .copy_from_slice(&texture.data[..src_row_size]);
                for bleed_y in 1..bleeding {
                    let (src, dst) = image
                        .data
                        .split_at_mut((min_y - bleeding + bleed_y) * page_row_size + (min_x - bleeding) * size);
                    dst[..src_row_size + 2 * bleeding * size].copy_from_slice(
                        &src[(min_y - bleeding + bleed_y - 1) * page_row_size + (min_x - bleeding) * size..
                            (min_y - bleeding + bleed_y - 1) * page_row_size + (max_x + bleeding) * size],
                    );
                }

                // Copy the actual image, while performing sideways bleeding.
                for (src_y, dst_y) in (min_y..max_y).enumerate() {
                    let dst_row = dst_y * page_row_size;
                    image.data[dst_row + min_x * size..dst_row + max_x * size]
                        .copy_from_slice(&texture.data[src_y * src_row_size..(src_y + 1) * src_row_size]);

                    for bleed_x in 0..bleeding {
                        let left_x = dst_y * page_row_size + (min_x - bleed_x - 1) * size;
                        image.data[left_x..left_x + size]
                            .copy_from_slice(&texture.data[src_y * src_row_size..src_y * src_row_size + size]);

                        let right_x = dst_y * page_row_size + (max_x + bleed_x) * size;
                        image.data[right_x..right_x + size]
                            .copy_from_slice(&texture.data[(src_y + 1) * src_row_size - size..(src_y + 1) * src_row_size]);
                    }
                }

                // Copy the bottom-most edge to bleed downwards.
                for bleed_y in 0..bleeding {
                    let (src, dst) = image
                        .data
                        .split_at_mut((max_y + bleed_y) * page_row_size + (min_x - bleeding) * size);
                    dst[..src_row_size + 2 * bleeding * size].copy_from_slice(
                        &src[(max_y + bleed_y - 1) * page_row_size + (min_x - bleeding) * size..
                            (max_y + bleed_y - 1) * page_row_size + (max_x + bleeding) * size],
                    );
                }

                atlas.sprite_map.insert(name, (atlas.pages.len(), sprites.len()));
                sprites.push(URect {
                    min: UVec2::new(min_x as u32, min_y as u32),
                    max: UVec2::new(max_x as u32, max_y as u32),
                });
            }

            let page_num = atlas.pages.len();
            atlas.pages.push(AtlasPage {
                image: load_context.add_labeled_asset(format!("page-{page_num}"), image),
                sprites,
            });

            Ok(())
        };

        'pages: while !entries.is_empty() {
            let mut packer = AtlasAllocator::new(size2(init_width as i32, init_height as i32));
            let mut ids = HashMap::<AllocId, (String, Image)>::new();

            while let Some((name, texture)) = entries.pop() {
                let UVec2 {
                    x: base_width,
                    y: base_height,
                } = texture.size();

                match packer.allocate(size2(
                    (base_width + 2 * padding as u32) as i32,
                    (base_height + 2 * padding as u32) as i32,
                )) {
                    Some(alloc) => {
                        ids.insert(alloc.id, (name, texture));
                    }
                    None => {
                        let Size2D { width, height, .. } = packer.size();
                        if width == max_width as i32 && height == max_height as i32 {
                            if packer.is_empty() {
                                return Err(TextureAtlasError::TooLarge {
                                    name,
                                    max_width,
                                    max_height,
                                    actual_width: width as u32,
                                    actual_height: height as u32,
                                });
                            } else {
                                end(ids, packer)?;

                                // Re-insert the entry to the back, since we didn't end up packing that one.
                                entries.push((name, texture));
                                continue 'pages;
                            }
                        }

                        let ChangeList { changes, failures } = packer.resize_and_rearrange(size2(
                            (width * 2).min(max_width as i32),
                            (height * 2).min(max_height as i32),
                        ));

                        if !failures.is_empty() {
                            unreachable!("resizing shouldn't cause rectangles to become unfittable")
                        }

                        let mut id_map = HashMap::new();
                        for Change { old, new } in changes {
                            let (name, texture) = ids.remove(&old.id).unwrap();
                            id_map.insert(new.id, (name, texture));
                        }

                        if !ids.is_empty() {
                            unreachable!("resizing should clear all old rectangles")
                        }

                        ids = id_map;
                    }
                }
            }

            end(ids, packer)?;
        }

        Ok(atlas)
    }

    #[inline]
    fn extensions(&self) -> &[&str] {
        &["atlas"]
    }
}
