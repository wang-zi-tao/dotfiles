use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    #[serde(default = "default_title")]
    pub title: String,
    #[serde(default = "default_listen")]
    pub listen: String,
    #[serde(default = "default_port")]
    pub port: u16,
    pub database: PathBuf,
    #[serde(default)]
    pub static_dir: Option<PathBuf>,
    #[serde(default)]
    pub auth: AuthConfig,
    #[serde(default = "default_search_engines")]
    pub search_engines: Vec<SearchEngine>,
    #[serde(default)]
    pub default_search_engine: Option<String>,
    #[serde(default)]
    pub intranet: Vec<IntranetGroup>,
    #[serde(default)]
    pub seed: SeedConfig,
    #[serde(default = "default_true")]
    pub secure_cookies: bool,
    /// 主页在公网上的挂载前缀（Caddy 用 strip_prefix 反代到这里）。
    /// 未登录跳转与 SSO 回跳都基于这个前缀拼 URL。
    #[serde(default = "default_base_path")]
    pub base_path: String,
    /// 主页的对外根地址，例如 `https://8.138.123.170`。
    ///
    /// SSO 的 `/api/verify` 跑在别的站点（dsh-web 的端口）上，如果 302 只给
    /// 相对路径 `/homepage/`，浏览器会把它解析到 dsh-web 自己的端口上，再次
    /// 触发 forward_auth，形成无限重定向。配置这个根地址后会跳绝对 URL。
    #[serde(default)]
    pub public_base_url: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AuthConfig {
    #[serde(default = "default_username")]
    pub username: String,
    #[serde(default)]
    pub password_hash_file: PathBuf,
    #[serde(default = "default_session_days")]
    pub session_days: u32,
}

impl Default for AuthConfig {
    fn default() -> Self {
        Self {
            username: default_username(),
            password_hash_file: PathBuf::new(),
            session_days: default_session_days(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SearchEngine {
    pub id: String,
    pub name: String,
    pub url: String,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SeedConfig {
    #[serde(default)]
    pub file: Option<PathBuf>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IntranetGroup {
    pub name: String,
    #[serde(default)]
    pub entries: Vec<IntranetEntry>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IntranetEntry {
    pub name: String,
    pub url: String,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub icon: Option<String>,
}

impl Config {
    pub fn load(path: &Path) -> Result<Self> {
        let raw = fs::read_to_string(path)
            .with_context(|| format!("读取配置文件失败: {}", path.display()))?;
        let mut config: Config = serde_json::from_str(&raw)
            .with_context(|| format!("解析配置文件失败: {}", path.display()))?;
        if config.search_engines.is_empty() {
            config.search_engines = default_search_engines();
        }
        Ok(config)
    }

    pub fn password_hash(&self) -> Result<String> {
        let path = &self.auth.password_hash_file;
        if path.as_os_str().is_empty() {
            anyhow::bail!("未配置 auth.passwordHashFile");
        }
        let hash = fs::read_to_string(path)
            .with_context(|| format!("读取密码哈希文件失败: {}", path.display()))?;
        let hash = hash.trim().to_string();
        if hash.is_empty() {
            anyhow::bail!("密码哈希文件为空: {}", path.display());
        }
        Ok(hash)
    }
}

fn default_title() -> String {
    "我的主页".to_string()
}

fn default_listen() -> String {
    "127.0.0.1".to_string()
}

fn default_port() -> u16 {
    18081
}

fn default_username() -> String {
    "wangzi".to_string()
}

fn default_session_days() -> u32 {
    30
}

fn default_true() -> bool {
    true
}

fn default_base_path() -> String {
    "/homepage".to_string()
}

pub fn default_search_engines() -> Vec<SearchEngine> {
    vec![
        SearchEngine {
            id: "google".into(),
            name: "Google".into(),
            url: "https://www.google.com/search?q={query}".into(),
        },
        SearchEngine {
            id: "bing".into(),
            name: "Bing".into(),
            url: "https://www.bing.com/search?q={query}".into(),
        },
        SearchEngine {
            id: "baidu".into(),
            name: "百度".into(),
            url: "https://www.baidu.com/s?wd={query}".into(),
        },
        SearchEngine {
            id: "duckduckgo".into(),
            name: "DuckDuckGo".into(),
            url: "https://duckduckgo.com/?q={query}".into(),
        },
        SearchEngine {
            id: "github".into(),
            name: "GitHub".into(),
            url: "https://github.com/search?q={query}".into(),
        },
        SearchEngine {
            id: "nixos".into(),
            name: "NixOS Search".into(),
            url: "https://search.nixos.org/packages?query={query}".into(),
        },
    ]
}
