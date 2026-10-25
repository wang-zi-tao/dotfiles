//! 本机网卡探测与邻居连通性/延迟测量。

use std::collections::HashSet;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::time::{Duration, Instant};

use nix::ifaddrs::getifaddrs;
use tracing::debug;

use crate::auth::Identity;
use crate::config::{Config, Node};
use crate::peer::PeerClient;
use crate::topology::{Link, LinkKind};

#[derive(Debug, Clone, Copy)]
pub struct LocalIface {
    pub ip: Ipv4Addr,
    pub prefix: u8,
}

/// 本机所有 IPv4 地址与掩码。
pub fn local_ipv4() -> Vec<LocalIface> {
    let mut out = Vec::new();
    let Ok(iter) = getifaddrs() else {
        return out;
    };
    for ifaddr in iter {
        let Some(addr) = ifaddr.address.as_ref().and_then(|a| a.as_sockaddr_in()) else {
            continue;
        };
        let ip = addr.ip();
        let prefix = ifaddr
            .netmask
            .as_ref()
            .and_then(|m| m.as_sockaddr_in())
            .map(|m| u32::from(m.ip()).leading_ones() as u8)
            .unwrap_or(32);
        out.push(LocalIface { ip, prefix });
    }
    out
}

pub fn local_ip_set() -> HashSet<Ipv4Addr> {
    local_ipv4().into_iter().map(|i| i.ip).collect()
}

pub fn same_subnet(a: Ipv4Addr, b: Ipv4Addr, prefix: u8) -> bool {
    let mask = if prefix == 0 {
        0
    } else {
        u32::MAX << (32 - prefix.min(32))
    };
    (u32::from(a) & mask) == (u32::from(b) & mask)
}

/// 探测目标地址；成功返回最小 RTT。
pub async fn probe_address(
    client: &PeerClient,
    identity: &Identity,
    node_name: &str,
    address: IpAddr,
    port: u16,
    timeout: Duration,
    attempts: usize,
) -> Option<f64> {
    let addr = SocketAddr::new(address, port);
    let mut best: Option<f64> = None;
    for _ in 0..attempts.max(1) {
        let started = Instant::now();
        let result = client
            .get_json::<serde_json::Value>(addr, node_name, identity, "/v1/ping", timeout)
            .await;
        match result {
            Ok(_) => {
                let rtt = started.elapsed().as_secs_f64() * 1000.0;
                best = Some(match best {
                    Some(prev) if prev < rtt => prev,
                    _ => rtt,
                });
            }
            Err(err) => {
                debug!(node = node_name, %addr, error = %err, "探测失败");
            }
        }
    }
    best
}

/// 一个节点可探测的地址候选（按优先级排序）。
///
/// 顺序：localIp（同网段记为 Lan）→ publicIp（Wan）→ clusterIp（Inferred，
/// 即经 WireGuard 隧道可达）。最后一项是兜底：DHCP 地址变动或跨网段时，
/// 仍能通过集群地址发现对端，避免出现单向边。
pub fn probe_candidates(node: &Node, local: &[LocalIface]) -> Vec<(Ipv4Addr, LinkKind)> {
    let mut out = Vec::new();
    for ip in node.local_ip.iter().copied() {
        let kind = if local.iter().any(|l| same_subnet(l.ip, ip, l.prefix)) {
            LinkKind::Lan
        } else {
            LinkKind::Wan
        };
        out.push((ip, kind));
    }
    for ip in node.public_ip.iter().copied() {
        out.push((ip, LinkKind::Wan));
    }
    if let Some(cluster_ip) = node.cluster_ip {
        out.push((cluster_ip, LinkKind::Inferred));
    }
    out
}

/// 按候选顺序探测一个节点，返回第一个可用直连。
pub async fn probe_node(
    client: &PeerClient,
    identity: &Identity,
    name: &str,
    node: &Node,
    service: &Config,
    local: &[LocalIface],
    timeout: Duration,
) -> Option<Link> {
    let port = node.peer_port(&service.service);
    let candidates = probe_candidates(node, local);
    for (ip, kind) in &candidates {
        if let Some(rtt) =
            probe_address(client, identity, name, IpAddr::V4(*ip), port, timeout, 2).await
        {
            if matches!(kind, LinkKind::Inferred) {
                debug!(
                    node = name,
                    address = %ip,
                    "localIp/publicIp 均不可达，改经 WireGuard 集群地址"
                );
            }
            return Some(Link {
                peer: name.to_string(),
                address: IpAddr::V4(*ip),
                port,
                rtt_ms: rtt,
                kind: *kind,
            });
        }
    }
    debug!(node = name, candidates = candidates.len(), "节点不可达");
    None
}

/// 并发探测所有节点，返回成功直连（顺序按配置）。
pub async fn probe_all(
    client: &PeerClient,
    identity: &Identity,
    config: &Config,
    timeout: Duration,
) -> Vec<Link> {
    let local = local_ipv4();
    let mut tasks = Vec::new();
    for (name, node) in config.peers() {
        let name = name.clone();
        let node = node.clone();
        let client = client.clone();
        let identity = identity.clone();
        let config = config.clone();
        let local = local.clone();
        tasks.push(tokio::spawn(async move {
            probe_node(&client, &identity, &name, &node, &config, &local, timeout).await
        }));
    }
    let mut links = Vec::new();
    for task in tasks {
        if let Ok(Some(link)) = task.await {
            links.push(link);
        }
    }
    links
}

/// 判断一个隧道目标是否允许：loopback、本机地址或显式允许列表。
pub fn target_allowed(config: &Config, ip: IpAddr) -> bool {
    let IpAddr::V4(v4) = ip else {
        return false;
    };
    if v4.is_loopback() {
        return true;
    }
    if local_ip_set().contains(&v4) {
        return true;
    }
    config.service.tunnel_allowed_targets.contains(&v4)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn candidates_include_cluster_ip_fallback() {
        let node: Node =
            serde_json::from_str(r#"{"localIp":["192.168.32.129"],"clusterIp":"192.168.16.13"}"#)
                .unwrap();
        let local = [LocalIface {
            ip: "192.168.32.1".parse().unwrap(),
            prefix: 24,
        }];
        let candidates = probe_candidates(&node, &local);
        assert_eq!(candidates.len(), 2);
        assert_eq!(candidates[0].0.to_string(), "192.168.32.129");
        assert!(matches!(candidates[0].1, LinkKind::Lan));
        assert_eq!(candidates[1].0.to_string(), "192.168.16.13");
        assert!(matches!(candidates[1].1, LinkKind::Inferred));
    }

    #[test]
    fn subnet_math() {
        let a: Ipv4Addr = "192.168.32.1".parse().unwrap();
        let b: Ipv4Addr = "192.168.32.128".parse().unwrap();
        let c: Ipv4Addr = "192.168.33.1".parse().unwrap();
        assert!(same_subnet(a, b, 24));
        assert!(!same_subnet(a, c, 24));
        assert!(same_subnet(a, c, 16));
    }
}
