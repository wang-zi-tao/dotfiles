//! UDP over HTTP/2：一个 h2 双向流承载一个 UDP 会话。
//!
//! 请求体是客户端发往 target 的帧，响应体是 target 回来的帧。

use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result};
use axum::body::Body;
use axum::response::Response;
use bytes::Bytes;
use futures_util::stream::{self, StreamExt};
use tokio::net::UdpSocket;
use tokio::sync::mpsc;
use tokio::time::timeout;

use crate::framing::{encode_frame, FrameReader};

/// 在 h2 流与一个 UDP 会话之间做双向转发。
pub async fn proxy(target: SocketAddr, body: Body, idle: Duration) -> Result<Response> {
    let socket = UdpSocket::bind((std::net::Ipv4Addr::UNSPECIFIED, 0))
        .await
        .context("创建 UDP 套接字失败")?;
    socket
        .connect(target)
        .await
        .with_context(|| format!("连接 UDP 目标 {target} 失败"))?;
    let socket = Arc::new(socket);

    let (tx, rx) = mpsc::channel::<Vec<u8>>(256);

    // 客户端 -> 目标
    let outbound = socket.clone();
    let mut incoming = body.into_data_stream();
    tokio::spawn(async move {
        let mut reader = FrameReader::new();
        while let Some(chunk) = incoming.next().await {
            let Ok(chunk) = chunk else { break };
            reader.feed(chunk);
            while let Some(payload) = reader.next_frame() {
                if outbound.send(&payload).await.is_err() {
                    return;
                }
            }
        }
    });

    // 目标 -> 客户端
    let inbound = socket.clone();
    tokio::spawn(async move {
        let mut buf = vec![0u8; 65535];
        loop {
            match timeout(idle, inbound.recv(&mut buf)).await {
                Ok(Ok(n)) => {
                    if tx.send(buf[..n].to_vec()).await.is_err() {
                        break;
                    }
                }
                Ok(Err(_)) => break,
                Err(_) => break,
            }
        }
    });

    let frames = stream::unfold(rx, |mut rx| async move {
        rx.recv().await.map(|payload| (payload, rx))
    })
    .map(|payload| Ok::<_, std::io::Error>(Bytes::from(encode_frame(&payload))));

    Ok(Response::new(Body::from_stream(frames)))
}
