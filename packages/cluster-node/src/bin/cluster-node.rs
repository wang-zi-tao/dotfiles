//! cluster-node 服务进程。

use std::path::PathBuf;

use anyhow::{bail, Context, Result};
use clap::Parser;
use tracing_subscriber::EnvFilter;

/// 集群节点互联服务：探测、拓扑、路由与隧道。
#[derive(Debug, Parser)]
#[command(
    name = "cluster-node",
    version,
    about = "集群节点互联服务：探测、拓扑、路由与 UDP 隧道"
)]
struct Args {
    /// 配置文件路径（JSON5）。
    #[arg(short, long, default_value = "/etc/cluster-node/config.json5")]
    config: PathBuf,

    /// 只校验配置，然后退出。
    #[arg(long)]
    check: bool,

    /// 打印本机 SSH host key 公钥（用于填进各节点的 hostPubKey）。
    #[arg(long)]
    print_host_key: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    if args.check {
        let config = cluster_node::config::Config::load(&args.config)?;
        println!(
            "配置有效：self = {}，节点 {} 个，主机名 {} 个",
            config.self_node,
            config.nodes.len(),
            config.hostnames.len()
        );
        return Ok(());
    }

    if args.print_host_key {
        let config = cluster_node::config::Config::load(&args.config)?;
        let identity = cluster_node::auth::Identity::load_or_generate(
            &config.self_node,
            &config.service.ssh_host_key,
            &config.service.state_dir,
        )
        .context("读取 SSH host key 失败")?;
        println!("{}", identity.public_openssh);
        eprintln!("指纹：{}", identity.fingerprint());
        if identity.generated {
            eprintln!(
                "注意：这是自动生成的临时 key（{}），请持久化后写进配置",
                identity.source.display()
            );
        }
        return Ok(());
    }

    if !args.config.exists() {
        bail!("配置文件 {} 不存在", args.config.display());
    }
    cluster_node::service::run(args.config).await
}
