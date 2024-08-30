use std::io::Error as IoError;

use bevy::{
    asset::{io::Reader, AssetLoader, AssetPath, AsyncReadExt, LoadContext, LoadDirectError, ParseAssetPathError},
    prelude::*,
    render::{
        render_asset::RenderAssetUsages,
        render_resource::{Extent3d, TextureDimension, TextureFormat},
        texture::TextureFormatPixelInfo,
    },
    utils::HashMap,
};
use guillotiere::{
    euclid::{Box2D, Size2D},
    size2, AllocId, AtlasAllocator, Change, ChangeList,
};
use ron::de::SpannedError;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Asset, Reflect, Debug, Clone)]
pub struct TextureAtlas {
    pub pages: Vec<AtlasPage>,
    pub texture_map: HashMap<String, (usize, usize)>,
}

#[derive(Reflect, Debug, Clone)]
pub struct AtlasPage {
    pub image: Handle<Image>,
    pub sprites: Vec<URect>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TextureAtlasFile {
    #[serde(default = "TextureAtlasFile::default_padding")]
    pub padding: u32,
    #[serde(default = "TextureAtlasFile::default_bleeding")]
    pub bleeding: u32,
    pub entries: Vec<TextureAtlasEntry>,
}

impl TextureAtlasFile {
    #[inline]
    pub const fn default_padding() -> u32 {
        4
    }

    #[inline]
    pub const fn default_bleeding() -> u32 {
        4
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum TextureAtlasEntry {
    File(String),
    Directory(String, Vec<TextureAtlasEntry>),
}

impl<T: ToString> From<T> for TextureAtlasEntry {
    #[inline]
    fn from(value: T) -> Self {
        Self::File(value.to_string())
    }
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
pub struct TextureAtlasSettings {
    pub init_width: u32,
    pub init_height: u32,
    pub max_width: u32,
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

#[derive(Error, Debug)]
pub enum TextureAtlasError {
    #[error("Texture '{name}' is too large: [{actual_width}, {actual_height}] > [{max_width}, {max_height}]")]
    TooLarge {
        name: String,
        max_width: u32,
        max_height: u32,
        actual_width: u32,
        actual_height: u32,
    },
    #[error("Sampler '{0}' doesn't exist")]
    NonexistentSampler(String),
    #[error("Texture '{name}' has an unsupported format: {format:?}")]
    UnsupportedFormat { name: String, format: TextureFormat },
    #[error(transparent)]
    InvalidImage(#[from] LoadDirectError),
    #[error(transparent)]
    InvalidPath(#[from] ParseAssetPathError),
    #[error(transparent)]
    InvalidFile(#[from] SpannedError),
    #[error(transparent)]
    Io(#[from] IoError),
}

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
            mut bleeding,
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

                    accum.push((
                        name.to_string_lossy().into_owned(),
                        load_context.loader().direct().load::<Image>(&path).await?.take(),
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
            texture_map: HashMap::new(),
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
                RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
            );
            let mut sprites = Vec::new();

            for (id, (name, texture)) in ids {
                let Box2D { min, max } = packer[id].to_usize();
                let min_x = min.x + padding;
                let min_y = min.y + padding;
                let max_x = max.x - padding;
                let max_y = max.y - padding;

                let target_row_size = (max_x - min_x) * size;

                let Some(texture) = texture.convert(TextureFormat::Rgba8UnormSrgb) else {
                    return Err(TextureAtlasError::UnsupportedFormat {
                        name,
                        format: texture.texture_descriptor.format,
                    })
                };

                for (src_y, dst_y) in (min_y..max_y).enumerate() {
                    image.data[dst_y * page_row_size + min_x * size..dst_y * page_row_size + max_x * size]
                        .copy_from_slice(&texture.data[src_y * target_row_size..(src_y + 1) * target_row_size]);
                }

                atlas.texture_map.insert(name, (atlas.pages.len(), sprites.len()));
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
                                })
                            } else {
                                end(ids, packer)?;

                                // Re-insert the entry to the back, since we didn't end up packing that one.
                                entries.push((name, texture));
                                continue 'pages
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
