//! 对等节点 HTTP/2 客户端：连接、签名请求、读取响应。

use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result};
use http_body_util::BodyExt;
use hyper::body::Bytes;
use hyper::client::conn::http2;
use hyper::{Request, StatusCode};
use hyper_util::rt::{TokioExecutor, TokioIo};
use serde::de::DeserializeOwned;
use tokio::net::TcpStream;
use tokio_rustls::TlsConnector;

use crate::auth::Identity;

/// 带连接超时的 HTTP/2 客户端。
#[derive(Clone)]
pub struct PeerClient {
    tls: Arc<rustls::ClientConfig>,
    connect_timeout: Duration,
}

impl PeerClient {
    pub fn new(connect_timeout: Duration) -> Result<Self> {
        Ok(Self {
            tls: crate::tls::client_config()?,
            connect_timeout,
        })
    }

    /// TCP + TLS(ALPN=h2) + HTTP/2 握手；连接任务在后台运行，返回请求发送端。
    pub async fn connect(
        &self,
        addr: SocketAddr,
        server_name: &str,
    ) -> Result<http2::SendRequest<axum::body::Body>> {
        let tcp = tokio::time::timeout(self.connect_timeout, TcpStream::connect(addr))
            .await
            .with_context(|| format!("连接 {addr} 超时"))??;
        let _ = tcp.set_nodelay(true);
        let name =
            rustls::pki_types::ServerName::try_from(server_name.to_string()).unwrap_or_else(|_| {
                rustls::pki_types::ServerName::try_from("cluster-node.invalid".to_string())
                    .expect("静态 server name")
            });
        let tls = tokio::time::timeout(
            self.connect_timeout,
            TlsConnector::from(self.tls.clone()).connect(name, tcp),
        )
        .await
        .with_context(|| format!("与 {addr} 的 TLS 握手超时"))??;
        let mut builder = http2::Builder::new(TokioExecutor::new());
        builder
            .timer(hyper_util::rt::TokioTimer::new())
            .keep_alive_interval(Duration::from_secs(15))
            .keep_alive_timeout(Duration::from_secs(10));
        let (sender, conn) = builder
            .handshake(TokioIo::new(tls))
            .await
            .with_context(|| format!("与 {addr} 的 HTTP/2 握手失败"))?;
        tokio::spawn(async move {
            if let Err(err) = conn.await {
                tracing::debug!(error = %err, "peer http2 connection closed");
            }
        });
        Ok(sender)
    }

    /// 发一个带认证头的 GET，返回 JSON。
    pub async fn get_json<T: DeserializeOwned>(
        &self,
        addr: SocketAddr,
        node: &str,
        identity: &Identity,
        path: &str,
        timeout: Duration,
    ) -> Result<T> {
        let mut sender = self.connect(addr, node).await?;
        let uri = format!("https://{node}{path}");
        let req = signed_request("GET", &uri, identity, axum::body::Body::empty())?;
        let resp = tokio::time::timeout(timeout, sender.send_request(req))
            .await
            .with_context(|| format!("请求 {node}{path} 超时"))??;
        let status = resp.status();
        let body = tokio::time::timeout(timeout, resp.into_body().collect())
            .await
            .with_context(|| format!("读取 {node}{path} 响应超时"))??;
        let bytes = body.to_bytes();
        if status != StatusCode::OK {
            anyhow::bail!(
                "{node}{path} 返回 {status}: {}",
                String::from_utf8_lossy(&bytes)
            );
        }
        serde_json::from_slice(&bytes).with_context(|| {
            format!(
                "解析 {node}{path} 响应失败: {}",
                String::from_utf8_lossy(&bytes)
            )
        })
    }

    pub async fn get_bytes(
        &self,
        addr: SocketAddr,
        node: &str,
        identity: &Identity,
        path: &str,
        timeout: Duration,
    ) -> Result<(StatusCode, Bytes)> {
        let mut sender = self.connect(addr, node).await?;
        let uri = format!("https://{node}{path}");
        let req = signed_request("GET", &uri, identity, axum::body::Body::empty())?;
        let resp = tokio::time::timeout(timeout, sender.send_request(req))
            .await
            .with_context(|| format!("请求 {node}{path} 超时"))??;
        let status = resp.status();
        let body = tokio::time::timeout(timeout, resp.into_body().collect())
            .await
            .with_context(|| format!("读取 {node}{path} 响应超时"))??;
        Ok((status, body.to_bytes()))
    }

    /// 发起流式请求（隧道用）：请求体是流，响应体保持打开直到对端结束。
    ///
    /// 注意：连接由 [`PeerClient::connect`] 里 spawn 的任务持有，因此这里
    /// 即使丢掉了 sender，流式响应体也会继续被传输，适合长连接隧道。
    pub async fn post_stream(
        &self,
        addr: SocketAddr,
        node: &str,
        identity: &Identity,
        uri: &str,
        body: axum::body::Body,
    ) -> Result<hyper::body::Incoming> {
        let mut sender = self.connect(addr, node).await?;
        let req = signed_request("POST", uri, identity, body)?;
        let resp = sender
            .send_request(req)
            .await
            .with_context(|| format!("发起隧道请求 {uri} 失败"))?;
        let status = resp.status();
        if !status.is_success() {
            anyhow::bail!("隧道请求 {uri} 返回 {status}");
        }
        Ok(resp.into_body())
    }
}

/// 构造带签名的请求。path 参与签名，query 不参与。
pub fn signed_request(
    method: &str,
    uri: &str,
    identity: &Identity,
    body: axum::body::Body,
) -> Result<Request<axum::body::Body>> {
    let parsed: hyper::Uri = uri.parse().with_context(|| format!("非法 URI {uri}"))?;
    let path = parsed.path().to_string();
    let headers = identity.headers(method, &path);
    Request::builder()
        .method(method)
        .uri(parsed)
        .header("x-cluster-node", headers.node)
        .header("x-cluster-time", headers.time.to_string())
        .header("x-cluster-nonce", headers.nonce)
        .header("x-cluster-signature", headers.signature)
        .body(body)
        .context("构造请求失败")
}

/// 把一段内存内容变成请求体。
pub fn full_body(bytes: impl Into<Bytes>) -> axum::body::Body {
    axum::body::Body::from(bytes.into())
}
