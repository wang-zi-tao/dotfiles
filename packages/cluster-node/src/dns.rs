//! 本地 DNS：把集群主机名解析到第一个可达节点的 clusterIp。
//!
//! 只实现解析客户端真正会发的查询（A / AAAA / 其他），足够给
//! /etc/resolv.conf 指向 127.0.0.1 的场景使用。

use std::net::{Ipv4Addr, SocketAddr};
use std::sync::Arc;

use anyhow::{Context, Result};
use tokio::net::UdpSocket;
use tokio::sync::RwLock;
use tracing::{debug, warn};

use crate::config::Config;
use crate::route::RouteTable;

const TYPE_A: u16 = 1;
const TYPE_AAAA: u16 = 28;

#[derive(Debug, Clone)]
pub struct Query {
    pub id: u16,
    pub rd: bool,
    pub name: String,
    pub qtype: u16,
    pub question: Vec<u8>,
}

/// 解析一个 DNS 查询报文；只支持单问题、无 EDNS。
pub fn parse_query(buf: &[u8]) -> Option<Query> {
    if buf.len() < 12 {
        return None;
    }
    let id = u16::from_be_bytes([buf[0], buf[1]]);
    let flags = u16::from_be_bytes([buf[2], buf[3]]);
    if flags & 0x8000 != 0 {
        return None;
    }
    let qdcount = u16::from_be_bytes([buf[4], buf[5]]);
    if qdcount != 1 {
        return None;
    }
    let (name, offset) = read_name(buf, 12)?;
    if buf.len() < offset + 4 {
        return None;
    }
    let qtype = u16::from_be_bytes([buf[offset], buf[offset + 1]]);
    Some(Query {
        id,
        rd: flags & 0x0100 != 0,
        name,
        qtype,
        question: buf[12..offset + 4].to_vec(),
    })
}

fn read_name(buf: &[u8], start: usize) -> Option<(String, usize)> {
    let mut labels: Vec<String> = Vec::new();
    let mut pos = start;
    let mut end: Option<usize> = None;
    let mut jumps = 0;
    loop {
        let len = *buf.get(pos)? as usize;
        if len & 0xC0 == 0xC0 {
            let second = *buf.get(pos + 1)? as usize;
            let target = ((len & 0x3F) << 8) | second;
            if end.is_none() {
                end = Some(pos + 2);
            }
            pos = target;
            jumps += 1;
            if jumps > 16 {
                return None;
            }
            continue;
        }
        if len == 0 {
            pos += 1;
            break;
        }
        let label = buf.get(pos + 1..pos + 1 + len)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        pos += 1 + len;
    }
    let name = labels.join(".").to_ascii_lowercase();
    Some((name, end.unwrap_or(pos)))
}

/// 构造响应报文；rcode 为 0 表示 NOERROR，3 表示 NXDOMAIN。
pub fn build_response(query: &Query, answers: &[Ipv4Addr], ttl: u32, rcode: u8) -> Vec<u8> {
    let mut out = Vec::with_capacity(64 + answers.len() * 16);
    out.extend_from_slice(&query.id.to_be_bytes());
    let mut flags: u16 = 0x8000 | 0x0400 | 0x0080 | (rcode as u16 & 0xF);
    if query.rd {
        flags |= 0x0100;
    }
    out.extend_from_slice(&flags.to_be_bytes());
    out.extend_from_slice(&1u16.to_be_bytes());
    out.extend_from_slice(&(answers.len() as u16).to_be_bytes());
    out.extend_from_slice(&0u16.to_be_bytes());
    out.extend_from_slice(&0u16.to_be_bytes());
    out.extend_from_slice(&query.question);
    for ip in answers {
        out.extend_from_slice(&[0xC0, 0x0C]);
        out.extend_from_slice(&TYPE_A.to_be_bytes());
        out.extend_from_slice(&1u16.to_be_bytes());
        out.extend_from_slice(&ttl.to_be_bytes());
        out.extend_from_slice(&4u16.to_be_bytes());
        out.extend_from_slice(&ip.octets());
    }
    out
}

/// 按 hostnames 里 hosts 的顺序，返回第一个可达节点的 clusterIp。
pub fn resolve(name: &str, config: &Config, routes: &RouteTable) -> Option<Ipv4Addr> {
    let name = name.trim_end_matches('.').to_ascii_lowercase();
    let entry = config.hostnames.get(&name)?;
    for host in &entry.hosts {
        if !routes.reachable(host) {
            continue;
        }
        if let Some(ip) = config.node(host).and_then(|n| n.cluster_ip) {
            return Some(ip);
        }
    }
    None
}

/// 回答一个查询报文。
pub fn answer(buf: &[u8], config: &Config, routes: &RouteTable, ttl: u32) -> Option<Vec<u8>> {
    let query = parse_query(buf)?;
    if query.qtype == TYPE_AAAA {
        // 集群只有 IPv4，明确回空答案，让客户端立刻回退到 A。
        return Some(build_response(&query, &[], ttl, 0));
    }
    if query.qtype != TYPE_A {
        return Some(build_response(&query, &[], ttl, 0));
    }
    match resolve(&query.name, config, routes) {
        Some(ip) => Some(build_response(&query, &[ip], ttl, 0)),
        None => {
            if config.hostnames.contains_key(&query.name) {
                // 主机名存在但当前没有可达节点：回 NOERROR + 空答案，
                // 客户端下次重试时可能就通了。
                Some(build_response(&query, &[], ttl, 0))
            } else {
                Some(build_response(&query, &[], ttl, 3))
            }
        }
    }
}

pub struct DnsService {
    pub config: Arc<Config>,
    pub routes: Arc<RwLock<RouteTable>>,
}

/// 在 bind 上跑 DNS 服务，直到进程退出。
pub async fn serve(bind: SocketAddr, service: DnsService) -> Result<()> {
    let socket = UdpSocket::bind(bind).await.with_context(|| {
        format!(
            "绑定 DNS {bind} 失败（端口可能被 avahi/mDNS 或 systemd-resolved 占用，可改 service.dns.port）"
        )
    })?;
    let socket = Arc::new(socket);
    let ttl = service.config.service.dns.ttl_secs;
    debug!(%bind, "dns listening");

    loop {
        let mut buf = vec![0u8; 1500];
        let (len, peer) = socket.recv_from(&mut buf).await?;
        let query = buf[..len].to_vec();
        let socket = socket.clone();
        let config = service.config.clone();
        let routes = service.routes.clone();
        tokio::spawn(async move {
            let table = routes.read().await;
            match answer(&query, &config, &table, ttl) {
                Some(response) => {
                    if let Err(err) = socket.send_to(&response, peer).await {
                        warn!(%peer, error = %err, "dns reply failed");
                    }
                }
                None => debug!(%peer, "dns: 忽略无法解析的报文"),
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;

    fn config() -> Config {
        Config::parse(
            r#"{
                self: "a",
                nodes: {
                    a: { clusterIp: "192.168.16.1" },
                    b: { clusterIp: "192.168.16.2" },
                },
                hostnames: { "web.c.wg": { hosts: ["b"] } },
            }"#,
        )
        .unwrap()
    }

    fn query(name: &str, qtype: u16) -> Vec<u8> {
        let mut out = vec![
            0x12, 0x34, 0x01, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        for label in name.split('.') {
            out.push(label.len() as u8);
            out.extend_from_slice(label.as_bytes());
        }
        out.push(0);
        out.extend_from_slice(&qtype.to_be_bytes());
        out.extend_from_slice(&1u16.to_be_bytes());
        out
    }

    #[test]
    fn parses_query_name() {
        let q = parse_query(&query("web.c.wg", 1)).unwrap();
        assert_eq!(q.name, "web.c.wg");
        assert_eq!(q.qtype, 1);
        assert!(q.rd);
    }

    #[test]
    fn answers_with_reachable_node_ip() {
        let cfg = config();
        let mut routes = RouteTable {
            self_node: "a".into(),
            generated_at: 0,
            nodes: Default::default(),
        };
        routes.nodes.insert(
            "b".into(),
            crate::route::RouteEntry {
                node: "b".into(),
                reachable: true,
                next_hop: Some("b".into()),
                path: vec!["a".into(), "b".into()],
                cost_ms: 1.0,
                rtt_ms: 1.0,
                address: Some("10.0.0.2".into()),
                kind: None,
                cluster_ip: Some("192.168.16.2".into()),
            },
        );
        let response = answer(&query("web.c.wg", 1), &cfg, &routes, 5).unwrap();
        assert_eq!(&response[0..2], &[0x12, 0x34]);
        assert_eq!(u16::from_be_bytes([response[6], response[7]]), 1);
        assert_eq!(&response[response.len() - 4..], &[192, 168, 16, 2]);
    }

    #[test]
    fn unknown_name_is_nxdomain() {
        let cfg = config();
        let routes = RouteTable {
            self_node: "a".into(),
            generated_at: 0,
            nodes: Default::default(),
        };
        let response = answer(&query("nope.c.wg", 1), &cfg, &routes, 5).unwrap();
        assert_eq!(response[3] & 0x0F, 3);
    }

    #[test]
    fn aaaa_returns_empty_noerror() {
        let cfg = config();
        let routes = RouteTable {
            self_node: "a".into(),
            generated_at: 0,
            nodes: Default::default(),
        };
        let response = answer(&query("web.c.wg", 28), &cfg, &routes, 5).unwrap();
        assert_eq!(response[3] & 0x0F, 0);
        assert_eq!(u16::from_be_bytes([response[6], response[7]]), 0);
    }
}
