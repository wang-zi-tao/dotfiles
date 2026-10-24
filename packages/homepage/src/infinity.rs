use std::collections::HashSet;
use std::fs;
use std::path::Path;

use anyhow::{anyhow, Context, Result};
use serde::Serialize;
use url::Url;

use crate::api::AppError;

pub const DEFAULT_CATEGORY: &str = "常用";

#[derive(Clone, Debug, Serialize)]
pub struct ParsedBookmark {
    pub name: String,
    pub url: String,
    pub icon: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParsedCategory {
    pub name: String,
    pub bookmarks: Vec<ParsedBookmark>,
}

#[derive(Clone, Debug, Default)]
pub struct Parsed {
    pub categories: Vec<ParsedCategory>,
}

pub fn parse_file(path: &Path) -> Result<Parsed, AppError> {
    let data = fs::read(path).map_err(|err| {
        AppError::internal(format!("读取 pages.infinity 失败 {}: {err}", path.display()))
    })?;
    parse(&data).map_err(|err| AppError::internal(format!("解析 pages.infinity 失败: {err}")))
}

pub fn parse(bytes: &[u8]) -> Result<Parsed> {
    let root: serde_json::Value =
        serde_json::from_slice(bytes).context("不是合法的 JSON 文件")?;
    let groups = root
        .pointer("/data/site/sites")
        .and_then(|value| value.as_array())
        .ok_or_else(|| anyhow!("缺少 data.site.sites 数组"))?;

    let mut builder = Builder::new();
    for group in groups {
        if let Some(items) = group.as_array() {
            for item in items {
                builder.walk(item, None);
            }
        }
    }
    Ok(builder.finish())
}

struct Builder {
    categories: Vec<ParsedCategory>,
    used_categories: HashSet<String>,
    seen_urls: HashSet<String>,
}

impl Builder {
    fn new() -> Self {
        let mut used_categories = HashSet::new();
        // 顶层散落站点使用“常用”分类，避免文件夹重名时冲突。
        used_categories.insert(DEFAULT_CATEGORY.to_string());
        Self {
            categories: vec![ParsedCategory {
                name: DEFAULT_CATEGORY.to_string(),
                bookmarks: Vec::new(),
            }],
            used_categories,
            seen_urls: HashSet::new(),
        }
    }

    fn finish(mut self) -> Parsed {
        self.categories.retain(|category| !category.bookmarks.is_empty());
        Parsed {
            categories: self.categories,
        }
    }

    fn walk(&mut self, item: &serde_json::Value, category: Option<&str>) {
        if let Some(children) = item.get("children").and_then(|value| value.as_array()) {
            if !children.is_empty() {
                let raw_name = item
                    .get("name")
                    .and_then(|value| value.as_str())
                    .unwrap_or("")
                    .trim();
                let child_category = self.unique_category(raw_name, category);
                for child in children {
                    self.walk(child, Some(&child_category));
                }
                return;
            }
        }
        self.add_bookmark(category.unwrap_or(DEFAULT_CATEGORY), item);
    }

    fn unique_category(&mut self, raw_name: &str, parent: Option<&str>) -> String {
        let base = if raw_name.is_empty() {
            "收藏夹"
        } else {
            raw_name
        };
        let base = match parent {
            Some(parent) => format!("{parent} / {base}"),
            None => base.to_string(),
        };
        let mut candidate = base.clone();
        let mut index = 2;
        while self.used_categories.contains(&candidate) {
            candidate = format!("{base} {index}");
            index += 1;
        }
        self.used_categories.insert(candidate.clone());
        self.categories.push(ParsedCategory {
            name: candidate.clone(),
            bookmarks: Vec::new(),
        });
        candidate
    }

    fn add_bookmark(&mut self, category: &str, item: &serde_json::Value) {
        let target = item
            .get("target")
            .and_then(|value| value.as_str())
            .unwrap_or("")
            .trim();
        let url = match normalize_target(target) {
            Some(url) => url,
            None => return,
        };
        if !self.seen_urls.insert(url.clone()) {
            return;
        }
        let raw_name = item
            .get("name")
            .and_then(|value| value.as_str())
            .unwrap_or("")
            .trim();
        let name = if raw_name.is_empty() {
            Url::parse(&url)
                .ok()
                .and_then(|parsed| parsed.host_str().map(str::to_string))
                .unwrap_or_else(|| url.clone())
        } else {
            collapse_whitespace(raw_name)
        };
        let icon = item
            .get("bgImage")
            .and_then(|value| value.as_str())
            .map(str::trim)
            .filter(|value| value.starts_with("http://") || value.starts_with("https://"))
            .map(str::to_string);

        if let Some(category) = self.categories.iter_mut().find(|c| c.name == category) {
            category.bookmarks.push(ParsedBookmark { name, url, icon });
        }
    }
}

fn normalize_target(raw: &str) -> Option<String> {
    if !(raw.starts_with("http://") || raw.starts_with("https://")) {
        return None;
    }
    let parsed = Url::parse(raw).ok()?;
    if !matches!(parsed.scheme(), "http" | "https") {
        return None;
    }
    Some(parsed.to_string())
}

fn collapse_whitespace(value: &str) -> String {
    value.split_whitespace().collect::<Vec<_>>().join(" ")
}
