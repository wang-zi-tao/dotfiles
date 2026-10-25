//! WireGuard 流量的 UDP-over-HTTP/2 自动代理（替代 udp2raw）。
//!
//! 每个 peer 一条「本地 UDP 端口 + h2 长连接」：
//!
//!   本机 wg0 --> 127.0.0.1:(peer.index + portBase) --> h2 --> 对端 cluster-node
//!   对端 cluster-node --> UDP(源端口同上) --> 对端 wg0 监听端口
//!
//! 本机把 wg0 里该 peer 的 endpoint 指向自己的这个本地端口，于是 WireGuard 的
//! 报文全部走 h2 长连接（自签 TLS + SSH host key 签名），避开公网 UDP 丢包。
//! 对端用同一个端口回写自己 wg0 的报文，因此两侧 endpoint 与源端口严格对应，
//! WireGuard 的 roaming 不会把连接切换到别的地址上。

use std::collections::{BTreeMap, BTreeSet};
use std::net::{Ipv4Addr, SocketAddr};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use tokio::net::UdpSocket;
use tracing::{info, warn};

use crate::config::{Config, Node, WgTunnel};
use crate::topology::LinkKind;

/// 默认端口基数，与 NixOS 里旧的 udp2raw channelPort 保持一致。
pub const DEFAULT_PORT_BASE: u16 = 41000;

/// 单个 peer 的隧道通道：本地 UDP 端口、socket 与统计。
pub struct Channel {
    pub peer: String,
    pub listen: SocketAddr,
    pub socket: Arc<UdpSocket>,
    pub stats: Arc<ChannelStats>,
}

#[derive(Default)]
pub struct ChannelStats {
    /// 从本机 WireGuard 读出、发往对端的报文数。
    pub to_peer: AtomicU64,
    /// 从对端收到、写回本机 WireGuard 的报文数。
    pub from_peer: AtomicU64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ChannelView {
    pub peer: String,
    pub listen: String,
    pub to_peer: u64,
    pub from_peer: u64,
    /// 当前是否已建立 h2 会话。
    pub session: bool,
}

/// 全部 peer 的隧道通道。
pub struct WgTunnelSet {
    channels: BTreeMap<String, Arc<Channel>>,
    /// 本机 WireGuard 监听端口（隧道的最终出口）。
    pub wg_port: u16,
}

/// 本机为某个 peer 绑定的端口：peer.index + portBase。
pub fn channel_port(node: &Node, base: u16) -> Option<u16> {
    node.wg_index().and_then(|index| index.checked_add(base))
}

/// 只有名字较小的一侧发起会话，避免双向各建一条造成重复投递。
pub fn is_initiator(self_node: &str, peer: &str) -> bool {
    self_node < peer
}

/// 会话发起方：同一条链路只允许一条会话（两侧都建会争抢同一个通道 socket 的读端）。
///
/// 公网可达性优先：本机在 NAT 后、对端有公网地址时由本机发起（否则对端连不进来）；
/// 反过来对端在 NAT 后时本机等待它连进来。两侧条件相同（都公网或都在 NAT 后）时，
/// 按名字顺序由名字较小的一侧发起。
pub fn initiates(config: &Config, peer: &str) -> bool {
    let self_has_public = !config.me().public_ip.is_empty();
    let peer_has_public = config
        .node(peer)
        .is_some_and(|node| !node.public_ip.is_empty());
    match (self_has_public, peer_has_public) {
        (false, true) => true,
        (true, false) => false,
        _ => is_initiator(&config.self_node, peer),
    }
}

/// 该链路是否应走 WireGuard 自动隧道。
///
/// 跨网段（Wan）链路默认代理：公网 UDP 丢包高，走 h2 长连接更稳。
/// 同局域网（Lan）链路默认直连，只有 proxyLan 打开时才一起代理。
/// 经 WireGuard 集群地址推断出的链路不代理，否则会绕圈。
pub fn should_proxy(wg: &WgTunnel, kind: LinkKind) -> bool {
    match kind {
        LinkKind::Wan => wg.enable,
        LinkKind::Lan => wg.enable && wg.proxy_lan,
        LinkKind::Tunnel => wg.enable,
        LinkKind::Inferred => false,
    }
}

impl WgTunnelSet {
    /// 绑定所有 peer 的本地端口；绑定失败的通道跳过并告警。
    pub fn bind(config: &Config) -> Result<Self> {
        let options = &config.service.wg_tunnel;
        let wg_port = config
            .me()
            .wg_port
            .context("service.wgTunnel.enable = true 时 self 节点必须有 wgPort")?;
        let base = options.port_base.max(1);
        let mut channels = BTreeMap::new();

        for (name, node) in config.peers() {
            if node.wg_public_key.is_none() || node.wg_port.is_none() {
                continue;
            }
            let Some(port) = channel_port(node, base) else {
                warn!(peer = %name, "节点缺少 WireGuard index，跳过它的隧道");
                continue;
            };
            let listen = SocketAddr::new(Ipv4Addr::LOCALHOST.into(), port);
            let socket = match std::net::UdpSocket::bind(listen) {
                Ok(socket) => socket,
                Err(err) => {
                    warn!(%listen, peer = %name, error = %err, "绑定 WireGuard 隧道端口失败，该节点继续直连");
                    continue;
                }
            };
            socket
                .set_nonblocking(true)
                .with_context(|| format!("设置 {listen} 非阻塞失败"))?;
            let socket =
                UdpSocket::from_std(socket).with_context(|| format!("注册 {listen} 失败"))?;
            info!(peer = %name, %listen, "WireGuard 隧道端口已绑定");
            channels.insert(
                name.clone(),
                Arc::new(Channel {
                    peer: name.clone(),
                    listen,
                    socket: Arc::new(socket),
                    stats: Arc::new(ChannelStats::default()),
                }),
            );
        }

        Ok(Self { channels, wg_port })
    }

    pub fn channel(&self, peer: &str) -> Option<&Arc<Channel>> {
        self.channels.get(peer)
    }

    pub fn channels(&self) -> impl Iterator<Item = &Arc<Channel>> {
        self.channels.values()
    }

    pub fn is_empty(&self) -> bool {
        self.channels.is_empty()
    }

    /// peer -> 本地端口，交给路由计划决定是否把 endpoint 切到隧道。
    pub fn ports(&self) -> BTreeMap<String, u16> {
        self.channels
            .iter()
            .map(|(name, channel)| (name.clone(), channel.listen.port()))
            .collect()
    }

    /// 隧道在本机的出口：127.0.0.1:wgPort。
    pub fn local_wg_addr(&self) -> SocketAddr {
        SocketAddr::new(Ipv4Addr::LOCALHOST.into(), self.wg_port)
    }

    pub fn views(&self, active: &BTreeSet<String>) -> Vec<ChannelView> {
        self.channels
            .values()
            .map(|channel| ChannelView {
                peer: channel.peer.clone(),
                listen: channel.listen.to_string(),
                to_peer: channel.stats.to_peer.load(Ordering::Relaxed),
                from_peer: channel.stats.from_peer.load(Ordering::Relaxed),
                session: active.contains(&channel.peer),
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;

    fn cfg() -> Config {
        Config::parse(
            r#"{
                self: "a",
                nodes: {
                    a: { clusterIp: "192.168.16.12", wgPort: 16538, wgPublicKey: "k-a" },
                    b: { clusterIp: "192.168.16.13", wgPort: 16538, wgPublicKey: "k-b", index: 13 },
                    c: { clusterIp: "192.168.16.2", wgPort: 21176, wgPublicKey: "k-c" },
                },
                service: { wgTunnel: { enable: true } },
            }"#,
        )
        .unwrap()
    }

    #[test]
    fn port_comes_from_peer_index() {
        let config = cfg();
        assert_eq!(channel_port(config.node("b").unwrap(), 41000), Some(41013));
        // 没有 index 时从 clusterIp 最后一段推导。
        assert_eq!(channel_port(config.node("c").unwrap(), 41000), Some(41002));
        assert_eq!(channel_port(config.node("a").unwrap(), 41000), Some(41012));
    }

    #[tokio::test]
    async fn binds_one_port_per_peer() {
        // 端口可能被本机其它进程占用，先挑一段空闲端口再验证。
        let mut base = 43000u16;
        loop {
            let free = [base + 2, base + 13]
                .iter()
                .all(|port| std::net::UdpSocket::bind(("127.0.0.1", *port)).is_ok());
            if free {
                break;
            }
            base = base.checked_add(100).expect("找不到空闲端口段");
        }
        let mut config = cfg();
        config.service.wg_tunnel.port_base = base;
        let set = WgTunnelSet::bind(&config).unwrap();
        assert_eq!(set.ports().len(), 2);
        assert_eq!(set.ports()["b"], base + 13);
        assert_eq!(set.local_wg_addr().port(), 16538);
        assert!(!set.is_empty());
    }

    #[test]
    fn smaller_name_initiates() {
        assert!(is_initiator("a", "b"));
        assert!(!is_initiator("b", "a"));
        assert!(!is_initiator("a", "a"));
    }

    #[test]
    fn nat_side_initiates_to_public_peer() {
        // 双方都没有公网地址：按名字顺序，名字小的发起。
        let config = cfg();
        assert!(initiates(&config, "b"));

        // 对方名字更小且双方都在 NAT 后：等对方发起。
        let config = Config::parse(
            r#"{
                self: "b",
                nodes: {
                    a: { clusterIp: "192.168.16.12", wgPort: 16538, wgPublicKey: "k-a" },
                    b: { clusterIp: "192.168.16.13", wgPort: 16538, wgPublicKey: "k-b" },
                },
                service: { wgTunnel: { enable: true } },
            }"#,
        )
        .unwrap();
        assert!(!initiates(&config, "a"));

        // 本机在 NAT 后、对端有公网地址：本机发起（即使名字更大）。
        let config = Config::parse(
            r#"{
                self: "wangzi-nuc",
                nodes: {
                    "wangzi-nuc": { clusterIp: "192.168.16.12", wgPort: 16538, wgPublicKey: "k-n" },
                    aliyun: { publicIp: "1.2.3.4", clusterIp: "192.168.16.3", wgPort: 40287, wgPublicKey: "k-c" },
                },
                service: { wgTunnel: { enable: true } },
            }"#,
        )
        .unwrap();
        assert!(initiates(&config, "aliyun"));

        // 反过来：本机有公网地址、对端在 NAT 后，等对端发起（名字顺序也不能反悔）。
        let config = Config::parse(
            r#"{
                self: "aliyun",
                nodes: {
                    "wangzi-nuc": { clusterIp: "192.168.16.12", wgPort: 16538, wgPublicKey: "k-n" },
                    aliyun: { publicIp: "1.2.3.4", clusterIp: "192.168.16.3", wgPort: 40287, wgPublicKey: "k-c" },
                },
                service: { wgTunnel: { enable: true } },
            }"#,
        )
        .unwrap();
        assert!(!initiates(&config, "wangzi-nuc"));
    }
}
