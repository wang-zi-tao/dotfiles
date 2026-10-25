//! 与本地服务通信的命令行客户端。
//!
//! CLI 与服务分离：CLI 只连接 unix socket，不直接读配置、不自己做探测。

use std::path::Path;

use anyhow::{bail, Context, Result};
use http_body_util::{BodyExt, Empty};
use hyper::body::Bytes;
use hyper::{Request, StatusCode};
use hyper_util::rt::TokioIo;
use serde_json::Value;
use tokio::net::UnixStream;

/// 向服务发一个请求，返回状态码与原始响应体。
pub async fn request(socket: &Path, method: &str, path: &str) -> Result<(StatusCode, Bytes)> {
    let stream = UnixStream::connect(socket)
        .await
        .with_context(|| format!("连接 {} 失败（服务启动了吗？）", socket.display()))?;
    let (mut sender, conn) = hyper::client::conn::http1::handshake(TokioIo::new(stream))
        .await
        .context("与服务做 HTTP/1.1 握手失败")?;
    tokio::spawn(async move {
        let _ = conn.await;
    });
    let req = Request::builder()
        .method(method)
        .uri(path)
        .header("host", "localhost")
        .body(Empty::<Bytes>::new())
        .context("构造请求失败")?;
    let resp = sender
        .send_request(req)
        .await
        .with_context(|| format!("请求 {path} 失败"))?;
    let status = resp.status();
    let body = resp
        .into_body()
        .collect()
        .await
        .context("读取响应失败")?
        .to_bytes();
    Ok((status, body))
}

/// GET 并解析成 JSON。
pub async fn get_json(socket: &Path, path: &str) -> Result<Value> {
    let (status, body) = request(socket, "GET", path).await?;
    if status != StatusCode::OK {
        bail!("服务返回 {status}: {}", String::from_utf8_lossy(&body));
    }
    serde_json::from_slice(&body)
        .with_context(|| format!("解析 {path} 响应失败: {}", String::from_utf8_lossy(&body)))
}

/// POST 并解析成 JSON。
pub async fn post_json(socket: &Path, path: &str) -> Result<Value> {
    let (status, body) = request(socket, "POST", path).await?;
    if status != StatusCode::OK {
        bail!("服务返回 {status}: {}", String::from_utf8_lossy(&body));
    }
    serde_json::from_slice(&body)
        .with_context(|| format!("解析 {path} 响应失败: {}", String::from_utf8_lossy(&body)))
}

/// 把 status 渲染成人类可读的表格。
pub fn render_status(value: &Value) -> String {
    let mut out = String::new();
    let node = value.get("node").and_then(Value::as_str).unwrap_or("?");
    let fingerprint = value
        .get("hostKeyFingerprint")
        .and_then(Value::as_str)
        .unwrap_or("?");
    let generated = value
        .get("generatedKey")
        .and_then(Value::as_bool)
        .unwrap_or(false);
    out.push_str(&format!("节点: {node}\n"));
    out.push_str(&format!(
        "Host key: {fingerprint}{}\n",
        if generated {
            "（自动生成的临时 key）"
        } else {
            ""
        }
    ));

    out.push_str(&format!(
        "网络事件: 已因网络变化重新探测 {} 次{}\n",
        value.get("netEvents").and_then(Value::as_u64).unwrap_or(0),
        match value
            .get("lastNetEvent")
            .and_then(|event| event.get("change"))
            .and_then(Value::as_str)
        {
            Some(change) => format!("，最近: {change}"),
            None => String::new(),
        }
    ));

    out.push_str("\n信息源（按信息源 + 版本号管理）:\n");
    match value.get("sources").and_then(Value::as_array) {
        Some(list) if !list.is_empty() => {
            for source in list {
                out.push_str(&format!(
                    "  {:<16} v{:<4} {:>6} 秒前  {} 条边{}\n",
                    source.get("source").and_then(Value::as_str).unwrap_or("?"),
                    source.get("version").and_then(Value::as_u64).unwrap_or(0),
                    source.get("ageSecs").and_then(Value::as_u64).unwrap_or(0),
                    source.get("edges").and_then(Value::as_u64).unwrap_or(0),
                    if source
                        .get("local")
                        .and_then(Value::as_bool)
                        .unwrap_or(false)
                    {
                        "  （本机）"
                    } else {
                        ""
                    },
                ));
            }
        }
        _ => out.push_str("  （无）\n"),
    }

    out.push_str("\n直连邻居:\n");
    if let Some(direct) = value.get("direct").and_then(Value::as_array) {
        if direct.is_empty() {
            out.push_str("  （无）\n");
        }
        for link in direct {
            out.push_str(&format!(
                "  {:<16} {:<15} rtt={:>7.2}ms  {:?}\n",
                link.get("node").and_then(Value::as_str).unwrap_or("?"),
                link.get("address").and_then(Value::as_str).unwrap_or("?"),
                link.get("rttMs").and_then(Value::as_f64).unwrap_or(0.0),
                link.get("kind").and_then(Value::as_str).unwrap_or("?"),
            ));
        }
    }

    out.push_str("\nWireGuard 隧道:\n");
    match value.get("wgTunnels").and_then(Value::as_array) {
        Some(list) if !list.is_empty() => {
            for channel in list {
                out.push_str(&format!(
                    "  {:<16} listen={:<21} 对端->本机={:<8} 本机->对端={:<8} {}\n",
                    channel.get("peer").and_then(Value::as_str).unwrap_or("?"),
                    channel.get("listen").and_then(Value::as_str).unwrap_or("?"),
                    channel.get("fromPeer").and_then(Value::as_u64).unwrap_or(0),
                    channel.get("toPeer").and_then(Value::as_u64).unwrap_or(0),
                    if channel
                        .get("session")
                        .and_then(Value::as_bool)
                        .unwrap_or(false)
                    {
                        "已连接"
                    } else {
                        "待连接"
                    },
                ));
            }
        }
        _ => out.push_str("  （无，未启用 wgTunnel）\n"),
    }

    out.push_str("\n路由表:\n");
    if let Some(nodes) = value
        .get("routes")
        .and_then(|r| r.get("nodes"))
        .and_then(Value::as_object)
    {
        for (name, entry) in nodes {
            let reachable = entry
                .get("reachable")
                .and_then(Value::as_bool)
                .unwrap_or(false);
            if !reachable {
                out.push_str(&format!("  {name:<16} 不可达\n"));
                continue;
            }
            let via = entry.get("nextHop").and_then(Value::as_str).unwrap_or("-");
            let cost = entry.get("costMs").and_then(Value::as_f64).unwrap_or(0.0);
            let path = entry
                .get("path")
                .and_then(Value::as_array)
                .map(|p| {
                    p.iter()
                        .filter_map(Value::as_str)
                        .collect::<Vec<_>>()
                        .join(" -> ")
                })
                .unwrap_or_default();
            out.push_str(&format!(
                "  {name:<16} via={via:<16} cost={cost:>9.1}ms  {path}\n"
            ));
        }
    }
    out
}

/// 把 graph 渲染成人类可读的「信息源 + 合并后的边」。
pub fn render_graph(value: &Value) -> String {
    let mut out = String::new();
    let self_node = value.get("selfNode").and_then(Value::as_str).unwrap_or("?");
    let version = value.get("version").and_then(Value::as_u64).unwrap_or(0);
    out.push_str(&format!("本机: {self_node}（v{version}）\n\n信息源:\n"));
    match value.get("sources").and_then(Value::as_array) {
        Some(list) if !list.is_empty() => {
            for source in list {
                out.push_str(&format!(
                    "  {:<16} v{:<4} {:>6} 秒前  {} 条边{}\n",
                    source.get("source").and_then(Value::as_str).unwrap_or("?"),
                    source.get("version").and_then(Value::as_u64).unwrap_or(0),
                    source.get("ageSecs").and_then(Value::as_u64).unwrap_or(0),
                    source
                        .get("edges")
                        .and_then(Value::as_array)
                        .map_or(0, Vec::len),
                    if source
                        .get("local")
                        .and_then(Value::as_bool)
                        .unwrap_or(false)
                    {
                        "  （本机）"
                    } else {
                        ""
                    },
                ));
            }
        }
        _ => out.push_str("  （无）\n"),
    }
    out.push_str("\n合并后的边:\n");
    match value.get("edges").and_then(Value::as_array) {
        Some(edges) if !edges.is_empty() => {
            for edge in edges {
                out.push_str(&format!(
                    "  {:<16} -- {:<16} rtt={:>8.2}ms  {:?}\n",
                    edge.get("a").and_then(Value::as_str).unwrap_or("?"),
                    edge.get("b").and_then(Value::as_str).unwrap_or("?"),
                    edge.get("rttMs").and_then(Value::as_f64).unwrap_or(0.0),
                    edge.get("kind").and_then(Value::as_str).unwrap_or("?"),
                ));
            }
        }
        _ => out.push_str("  （无）\n"),
    }
    out
}
