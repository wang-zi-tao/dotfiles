mod api;
mod auth;
mod config;
mod db;
mod infinity;
mod schema;
mod seed;

use std::io::Read;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{anyhow, Context, Result};
use clap::{Parser, Subcommand};
use tracing::{info, warn};
use tracing_subscriber::EnvFilter;

use crate::api::AppState;
use crate::config::Config;

#[derive(Parser, Debug)]
#[command(name = "homepage", version, about = "个人浏览器主页")]
struct Cli {
    /// 配置文件路径
    #[arg(long, default_value = "/etc/homepage/config.json", global = true)]
    config: PathBuf,
    /// 覆盖静态资源目录
    #[arg(long)]
    static_dir: Option<PathBuf>,
    /// 覆盖监听地址
    #[arg(long)]
    listen: Option<String>,
    /// 覆盖监听端口
    #[arg(long)]
    port: Option<u16>,
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// 从 stdin 读取密码并输出 bcrypt 哈希
    HashPassword,
    /// 把 pages.infinity 转换为简易收藏 JSON（后端只读取转换后的 JSON）
    ConvertInfinity {
        /// pages.infinity 文件路径
        input: PathBuf,
        /// 输出文件路径，缺省打印到 stdout
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| EnvFilter::new("homepage=info,tower_http=warn")),
        )
        .init();

    let cli = Cli::parse();

    match cli.command {
        Some(Command::HashPassword) => {
            let mut password = String::new();
            std::io::stdin().read_to_string(&mut password)?;
            let password = password.trim_end_matches(['\r', '\n']);
            if password.is_empty() {
                anyhow::bail!("密码不能为空");
            }
            println!("{}", auth::hash_password(password)?);
            return Ok(());
        }
        Some(Command::ConvertInfinity { input, output }) => {
            let parsed = infinity::parse_file(&input)?;
            let document = seed::SeedDocument::from_parsed(parsed);
            let json = serde_json::to_string_pretty(&document)?;
            match output {
                Some(output) => {
                    if let Some(parent) = output.parent() {
                        std::fs::create_dir_all(parent)?;
                    }
                    std::fs::write(&output, format!("{json}\n"))?;
                    eprintln!("已写入 {}", output.display());
                }
                None => println!("{json}"),
            }
            return Ok(());
        }
        None => {}
    }

    let mut config = Config::load(&cli.config)?;
    if let Some(listen) = cli.listen {
        config.listen = listen;
    }
    if let Some(port) = cli.port {
        config.port = port;
    }
    let static_dir = cli.static_dir.or_else(|| config.static_dir.clone());
    match &static_dir {
        Some(dir) if dir.join("index.html").is_file() => {}
        Some(dir) => warn!("静态资源目录缺少 index.html: {}", dir.display()),
        None => warn!("未配置静态资源目录，仅提供 API"),
    }

    let pool = db::open(&config.database)?;
    if let Some(seed) = config.seed.file.clone() {
        let mut conn = pool
            .get()
            .map_err(|err| anyhow!("获取数据库连接失败: {err}"))?;
        match db::seed_from_json(&mut conn, &seed) {
            Ok(0) => {}
            Ok(count) => info!("已从 {} 导入 {} 条收藏", seed.display(), count),
            Err(err) => warn!("导入初始收藏失败: {err}"),
        }
    }
    {
        let mut conn = pool
            .get()
            .map_err(|err| anyhow!("获取数据库连接失败: {err}"))?;
        db::purge_expired_sessions(&mut conn)?;
    }

    let state = AppState {
        config: Arc::new(config),
        db: pool,
        throttle: Arc::default(),
    };
    let addr = format!("{}:{}", state.config.listen, state.config.port);
    let listener = tokio::net::TcpListener::bind(&addr)
        .await
        .with_context(|| format!("监听 {addr} 失败"))?;
    info!("homepage 已启动: http://{addr}");

    axum::serve(listener, api::router(state, static_dir))
        .with_graceful_shutdown(shutdown_signal())
        .await
        .context("HTTP 服务异常退出")?;
    Ok(())
}

async fn shutdown_signal() {
    let _ = tokio::signal::ctrl_c().await;
    info!("收到退出信号，正在关闭");
}
