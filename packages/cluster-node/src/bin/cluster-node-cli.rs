//! cluster-node-cli：通过本地 unix socket 查询服务状态。

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};
use cluster_node::cli;

#[derive(Debug, Parser)]
#[command(
    name = "cluster-node-cli",
    version,
    about = "cluster-node 命令行：查询状态、网络图与路由，并强制重新探测"
)]
struct Args {
    /// 服务监听的 unix socket。
    #[arg(short, long, default_value = "/run/cluster-node/node.sock")]
    socket: PathBuf,

    /// 输出原始 JSON。
    #[arg(long, global = true)]
    json: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// 查看节点、直连邻居与路由表。
    Status,
    /// 查看网络图（所有已知边）。
    Graph,
    /// 查看路由表。
    Routes,
    /// 强制立刻重新探测网络环境并重算路由。
    Rescan,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let socket = args.socket.as_path();

    match args.command {
        Command::Status => {
            let value = cli::get_json(socket, "/v1/status").await?;
            if args.json {
                println!("{}", serde_json::to_string_pretty(&value)?);
            } else {
                print!("{}", cli::render_status(&value));
            }
        }
        Command::Graph => {
            let value = cli::get_json(socket, "/v1/graph").await?;
            if args.json {
                print_json(&value, true)?;
            } else {
                print!("{}", cli::render_graph(&value));
            }
        }
        Command::Routes => {
            let value = cli::get_json(socket, "/v1/routes").await?;
            print_json(&value, args.json)?;
        }
        Command::Rescan => {
            let value = cli::post_json(socket, "/v1/rescan").await?;
            if args.json {
                println!("{}", serde_json::to_string_pretty(&value)?);
            } else {
                let direct = value.get("direct").and_then(|v| v.as_u64()).unwrap_or(0);
                let edges = value.get("edges").and_then(|v| v.as_u64()).unwrap_or(0);
                println!("重新探测完成：直连 {direct} 个，间接边 {edges} 条");
                if let Some(err) = value.get("error").and_then(|v| v.as_str()) {
                    println!("警告：{err}");
                }
            }
        }
    }
    Ok(())
}

fn print_json(value: &serde_json::Value, compact: bool) -> Result<()> {
    if compact {
        println!("{}", serde_json::to_string(value)?);
    } else {
        println!("{}", serde_json::to_string_pretty(value)?);
    }
    Ok(())
}
