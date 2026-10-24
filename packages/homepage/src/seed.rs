use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

use crate::infinity::Parsed;

/// 简易收藏格式：由 `convert-infinity` 预先转换生成（bookmarks.json），后端只读取此格式。
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SeedDocument {
    #[serde(default = "default_version")]
    pub version: u32,
    #[serde(default)]
    pub categories: Vec<SeedCategory>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SeedCategory {
    pub name: String,
    #[serde(default)]
    pub bookmarks: Vec<SeedBookmark>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SeedBookmark {
    pub name: String,
    pub url: String,
    #[serde(default)]
    pub icon: Option<String>,
}

fn default_version() -> u32 {
    1
}

impl SeedDocument {
    pub fn from_parsed(parsed: Parsed) -> Self {
        Self {
            version: 1,
            categories: parsed
                .categories
                .into_iter()
                .map(|category| SeedCategory {
                    name: category.name,
                    bookmarks: category
                        .bookmarks
                        .into_iter()
                        .map(|bookmark| SeedBookmark {
                            name: bookmark.name,
                            url: bookmark.url,
                            icon: bookmark.icon,
                        })
                        .collect(),
                })
                .collect(),
        }
    }

    pub fn load(path: &Path) -> Result<Self> {
        let raw = fs::read_to_string(path)
            .with_context(|| format!("读取收藏 JSON 失败: {}", path.display()))?;
        let document: SeedDocument = serde_json::from_str(&raw)
            .with_context(|| format!("解析收藏 JSON 失败: {}", path.display()))?;
        Ok(document)
    }
}
