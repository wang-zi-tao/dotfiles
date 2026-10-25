//! cluster-node 配置：由 NixOS 集群配置生成，JSON5 格式。
//!
//! 顶层字段只保留 README 中约定的 self / nodes / hostnames，
//! 运行参数（端口、状态目录、DNS、隧道等）放在可选的 service 段，
//! 全部有默认值，因此最小配置就是 README 里那三段。

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::net::{Ipv4Addr, SocketAddr};
use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Deserializer, Serialize};

/// 集群配置。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    /// 本节点名字，必须出现在 nodes 中。
    #[serde(rename = "self")]
    pub self_node: String,
    pub nodes: BTreeMap<String, Node>,
    /// 集群内主机名到候选节点的映射，DNS 按 hosts 顺序选择第一个可联通节点。
    #[serde(default)]
    pub hostnames: BTreeMap<String, Hostname>,
    #[serde(default)]
    pub service: Service,
}

/// 单个节点。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Node {
    /// 局域网地址，可以是一个字符串或字符串数组。
    #[serde(default)]
    pub local_ip: Addresses,
    /// 公网地址，可以是一个字符串或字符串数组。
    #[serde(default)]
    pub public_ip: Addresses,
    /// 链路速度（MB/s），用于计算路由权重；缺省按 1000 MB/s 处理。
    #[serde(default)]
    pub speed: Option<f64>,
    /// 节点 SSH host public key（ed25519），用于双向身份认证。
    #[serde(default)]
    pub host_pub_key: String,
    /// WireGuard 集群地址（如 192.168.16.12）。
    #[serde(default)]
    pub cluster_ip: Option<Ipv4Addr>,
    /// WireGuard 公钥（base64），用于下发 allowed-ips。
    #[serde(default)]
    pub wg_public_key: Option<String>,
    /// WireGuard 监听端口。
    #[serde(default)]
    pub wg_port: Option<u16>,
    /// 本服务在该节点上的监听端口，缺省用 service.peerPort。
    #[serde(default)]
    pub port: Option<u16>,
    /// Nix 为该节点配置的额外网段（cluster.wireguard.nodes.<n>.config.clusterIpRange，
    /// 通常是 10.16.<index>.0/24）。内核里的 allowed-ips 被改写后用它兜底恢复。
    #[serde(default)]
    pub cluster_ip_range: Option<String>,
    /// WireGuard 索引（cluster.wireguard.nodes.<n>.config.index），
    /// 用于推导本机为该节点绑定的隧道端口。
    #[serde(default)]
    pub index: Option<u16>,
}

impl Node {
    pub fn peer_port(&self, service: &Service) -> u16 {
        self.port.unwrap_or(service.peer_port)
    }

    pub fn speed(&self) -> f64 {
        match self.speed {
            Some(s) if s.is_finite() && s > 0.0 => s,
            _ => 1000.0,
        }
    }

    /// WireGuard 索引：优先用配置里的 index，否则取 clusterIp 的最后一段。
    pub fn wg_index(&self) -> Option<u16> {
        self.index
            .or_else(|| self.cluster_ip.map(|ip| ip.octets()[3] as u16))
    }

    pub fn all_addresses(&self) -> Vec<Ipv4Addr> {
        let mut out = Vec::new();
        out.extend(self.local_ip.iter().copied());
        out.extend(self.public_ip.iter().copied());
        out
    }
}

/// 地址列表，接受 "1.2.3.4" 或 ["1.2.3.4", "5.6.7.8"]。
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Addresses(pub Vec<Ipv4Addr>);

impl Addresses {
    pub fn iter(&self) -> std::slice::Iter<'_, Ipv4Addr> {
        self.0.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for Addresses {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, ip) in self.0.iter().enumerate() {
            if idx > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{ip}")?;
        }
        Ok(())
    }
}

impl Serialize for Addresses {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(ser)
    }
}

impl<'de> Deserialize<'de> for Addresses {
    fn deserialize<D: Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum OneOrMany {
            One(String),
            Many(Vec<String>),
        }

        let raw = OneOrMany::deserialize(de)?;
        let items = match raw {
            OneOrMany::One(s) => vec![s],
            OneOrMany::Many(v) => v,
        };
        let mut out = Vec::with_capacity(items.len());
        for item in items {
            let ip: Ipv4Addr = item.trim().parse().map_err(serde::de::Error::custom)?;
            out.push(ip);
        }
        Ok(Addresses(out))
    }
}

/// 集群主机名。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Hostname {
    /// 优先级从高到低的候选节点名。
    pub hosts: Vec<String>,
}

/// 运行时参数，全部有默认值。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", default)]
pub struct Service {
    /// 对等节点 API 端口（https/http2）。
    pub peer_port: u16,
    /// 对等节点 API 监听地址。
    pub bind: Ipv4Addr,
    /// 本地 CLI 使用的 unix socket。
    pub socket: PathBuf,
    /// 状态目录：自签证书、wg 计划等。
    pub state_dir: PathBuf,
    /// 本机 SSH host key（ed25519），缺省时在 stateDir 自动生成一把临时 key。
    pub ssh_host_key: PathBuf,
    /// 自签证书路径，缺省为 stateDir/tls/cert.pem。
    pub tls_cert: Option<PathBuf>,
    /// 自签私钥路径，缺省为 stateDir/tls/key.pem。
    pub tls_key: Option<PathBuf>,
    /// 探测间隔。
    pub probe_interval_ms: u64,
    /// 单次探测超时。
    pub probe_timeout_ms: u64,
    /// 与邻居交换网络图的间隔。
    pub gossip_interval_ms: u64,
    /// 重算路由的间隔。
    pub route_interval_ms: u64,
    /// 网络变化（链路/地址）时立即触发重新探测。
    pub net_watch: NetWatch,
    /// 允许的时钟偏差（秒）。
    pub auth_skew_secs: u64,
    /// 是否把 wg/ip 计划真正下发到内核。
    pub apply_kernel: bool,
    /// 是否管理 WireGuard endpoint（关闭时只管理 allowed-ips 与路由）。
    pub manage_endpoints: bool,
    /// WireGuard 接口名。
    pub wg_interface: String,
    /// WireGuard 的 UDP-over-HTTP/2 自动代理（替代 udp2raw）。
    pub wg_tunnel: WgTunnel,
    /// 本地 DNS 服务。
    pub dns: Dns,
    /// UDP over HTTP/2 隧道。
    pub tunnels: Vec<Tunnel>,
    /// 隧道目标允许列表（除 loopback / 本机地址外的额外目标）。
    pub tunnel_allowed_targets: Vec<Ipv4Addr>,
}

impl Default for Service {
    fn default() -> Self {
        Self {
            peer_port: 7788,
            bind: Ipv4Addr::UNSPECIFIED,
            socket: PathBuf::from("/run/cluster-node/node.sock"),
            state_dir: PathBuf::from("/var/lib/cluster-node"),
            ssh_host_key: PathBuf::from("/etc/ssh/ssh_host_ed25519_key"),
            tls_cert: None,
            tls_key: None,
            probe_interval_ms: 3000,
            probe_timeout_ms: 800,
            gossip_interval_ms: 15000,
            route_interval_ms: 5000,
            net_watch: NetWatch::default(),
            auth_skew_secs: 120,
            apply_kernel: false,
            manage_endpoints: false,
            wg_interface: "wg0".to_string(),
            wg_tunnel: WgTunnel::default(),
            dns: Dns::default(),
            tunnels: Vec::new(),
            tunnel_allowed_targets: Vec::new(),
        }
    }
}

/// WireGuard 的 UDP-over-HTTP/2 自动代理。
///
/// 开启后本机为每个 peer 绑定 127.0.0.1:(peer.index + portBase)，
/// 把 WireGuard 的 peer endpoint 指向该端口：本机 WireGuard 发出的包
/// 由该端口读出、经 h2 长连接送到对端；对端写回本机 WireGuard 监听端口。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", default)]
pub struct WgTunnel {
    /// 是否启用自动代理。
    pub enable: bool,
    /// 端口基数（与旧的 udp2raw channelPort 一致，默认 41000）。
    pub port_base: u16,
    /// 是否也代理同一局域网（Lan）内的直连邻居；默认只代理跨网段（Wan）链路。
    pub proxy_lan: bool,
}

impl Default for WgTunnel {
    fn default() -> Self {
        Self {
            enable: false,
            port_base: 41000,
            proxy_lan: false,
        }
    }
}

/// 本地 DNS。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", default)]
pub struct Dns {
    pub enable: bool,
    pub bind: Ipv4Addr,
    pub port: u16,
    pub ttl_secs: u32,
}

impl Default for Dns {
    fn default() -> Self {
        Self {
            enable: true,
            bind: Ipv4Addr::LOCALHOST,
            port: 5335,
            ttl_secs: 5,
        }
    }
}

/// 网络变化监听：内核 netlink 事件（链路 UP/DOWN、IP 地址增删）触发重新探测。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", default)]
pub struct NetWatch {
    /// 是否启用；关闭后只能靠 probeIntervalMs 定时发现网络变化。
    pub enable: bool,
    /// 防抖时长（毫秒）：同一波事件合并成一次重新探测。
    pub debounce_ms: u64,
}

impl Default for NetWatch {
    fn default() -> Self {
        Self {
            enable: true,
            // 一次 wifi 重连 / DHCP 续租通常会连发 link+addr 多条事件，
            // 1500ms 的静默窗口足以把同一波变化合并成一次探测。
            debounce_ms: 1500,
        }
    }
}

/// 一条 UDP over HTTP/2 隧道：本地 listen 收到的报文经 via 节点转到 target。
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Tunnel {
    pub listen: SocketAddr,
    /// 中转节点名。
    pub via: String,
    /// 目标地址，必须是目标节点本机地址（loopback / 本机网卡 / 允许列表）。
    pub target: SocketAddr,
}

impl Config {
    pub fn load(path: &Path) -> Result<Self> {
        let text = std::fs::read_to_string(path)
            .with_context(|| format!("读取配置 {} 失败", path.display()))?;
        let config: Config =
            json5::from_str(&text).with_context(|| format!("解析配置 {} 失败", path.display()))?;
        config.validate()?;
        Ok(config)
    }

    pub fn parse(text: &str) -> Result<Self> {
        let config: Config = json5::from_str(text).context("解析配置失败")?;
        config.validate()?;
        Ok(config)
    }

    pub fn node(&self, name: &str) -> Option<&Node> {
        self.nodes.get(name)
    }

    pub fn me(&self) -> &Node {
        self.nodes
            .get(&self.self_node)
            .expect("validate() 保证 self 节点存在")
    }

    /// 结构校验：节点引用、地址与 key 格式、clusterIp 唯一性。
    pub fn validate(&self) -> Result<()> {
        if self.self_node.trim().is_empty() {
            bail!("配置缺少 self");
        }
        if !self.nodes.contains_key(&self.self_node) {
            bail!("self = {:?} 不在 nodes 中", self.self_node);
        }
        if self.nodes.is_empty() {
            bail!("nodes 为空");
        }
        for (name, node) in &self.nodes {
            if name.trim().is_empty() {
                bail!("节点名不能为空");
            }
            if !node.host_pub_key.is_empty() {
                crate::auth::parse_ed25519_public(&node.host_pub_key)
                    .with_context(|| format!("节点 {name} 的 hostPubKey 无效"))?;
            }
            if node.speed.is_some_and(|s| !s.is_finite() || s <= 0.0) {
                bail!("节点 {name} 的 speed 必须为正数");
            }
            if node.peer_port(&self.service) == 0 {
                bail!("节点 {name} 的端口不能为 0");
            }
        }
        let mut cluster_ips = BTreeSet::new();
        for (name, node) in &self.nodes {
            if let Some(ip) = node.cluster_ip {
                if !cluster_ips.insert(ip) {
                    bail!("clusterIp {ip} 重复（节点 {name}）");
                }
            }
        }
        for (name, host) in &self.hostnames {
            if host.hosts.is_empty() {
                bail!("hostnames.{name}.hosts 为空");
            }
            for node in &host.hosts {
                if !self.nodes.contains_key(node) {
                    bail!("hostnames.{name} 引用了未知节点 {node}");
                }
            }
        }
        if self.service.peer_port == 0 {
            bail!("service.peerPort 不能为 0");
        }
        if self.service.probe_interval_ms == 0 || self.service.probe_timeout_ms == 0 {
            bail!("probeIntervalMs / probeTimeoutMs 必须大于 0");
        }
        if self.service.wg_tunnel.enable {
            if self.service.wg_tunnel.port_base == 0 {
                bail!("service.wgTunnel.portBase 不能为 0");
            }
            if self.me().wg_port.is_none() {
                bail!("service.wgTunnel.enable = true 时 self 节点必须有 wgPort");
            }
        }
        for tunnel in &self.service.tunnels {
            if !self.nodes.contains_key(&tunnel.via) {
                bail!("tunnels.via 引用了未知节点 {}", tunnel.via);
            }
        }
        Ok(())
    }

    /// 返回除自己以外的节点，按名字排序。
    pub fn peers(&self) -> impl Iterator<Item = (&String, &Node)> {
        self.nodes
            .iter()
            .filter(move |(name, _)| *name != &self.self_node)
    }

    /// CLI / API 输出用：隐藏 key 的安全摘要。
    pub fn host_key_summary(&self, node: &str) -> Option<String> {
        self.nodes.get(node).map(|n| {
            if n.host_pub_key.is_empty() {
                "<none>".to_string()
            } else {
                crate::auth::public_key_fingerprint(&n.host_pub_key)
                    .unwrap_or_else(|_| "<invalid>".into())
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const README_EXAMPLE: &str = r#"
    {
      self: "wangzi-nuc",
      nodes: {
        "wangzi-nuc": {
          localIp: ["192.168.32.1", "10.0.0.2"],
          publicIp: [],
          speed: 100,
          hostPubKey: "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINrupNtnvcH2k0jqB1r6tM2ZyF8UL1g/raeltFJnWn8y test@example",
          clusterIp: "192.168.16.12",
          wgPublicKey: "Vk2vw8TbtI7GgktauuppvfhKAAxyEeNC8+/nxt10t1s=",
          wgPort: 16538,
        },
        "aliyun-hk": {
          localIp: "10.0.0.3",
          publicIp: "47.83.14.140",
          speed: 5,
          hostPubKey: "",
          clusterIp: "192.168.16.2",
        },
      },
      hostnames: {
        "ollama.c.wg": { hosts: ["aliyun-hk", "wangzi-nuc"] },
      },
    }
    "#;

    #[test]
    fn parses_readme_example() {
        let cfg = Config::parse(README_EXAMPLE).unwrap();
        assert_eq!(cfg.self_node, "wangzi-nuc");
        assert_eq!(cfg.nodes["wangzi-nuc"].local_ip.0.len(), 2);
        assert_eq!(
            cfg.nodes["aliyun-hk"].local_ip.0[0],
            Ipv4Addr::new(10, 0, 0, 3)
        );
        assert_eq!(
            cfg.nodes["aliyun-hk"].public_ip.0[0],
            Ipv4Addr::new(47, 83, 14, 140)
        );
        assert_eq!(cfg.nodes["aliyun-hk"].speed(), 5.0);
        assert_eq!(cfg.me().cluster_ip, Some(Ipv4Addr::new(192, 168, 16, 12)));
        assert_eq!(cfg.service.peer_port, 7788);
        assert_eq!(cfg.hostnames["ollama.c.wg"].hosts[1], "wangzi-nuc");
    }

    #[test]
    fn rejects_dangling_hostname() {
        let text = r#"{ self: "a", nodes: { a: {} }, hostnames: { x: { hosts: ["b"] } } }"#;
        let err = Config::parse(text).unwrap_err().to_string();
        assert!(err.contains("未知节点 b"), "{err}");
    }

    #[test]
    fn rejects_duplicate_cluster_ip() {
        let text = r#"{
            self: "a",
            nodes: {
                a: { clusterIp: "192.168.16.1" },
                b: { clusterIp: "192.168.16.1" },
            },
        }"#;
        assert!(Config::parse(text)
            .unwrap_err()
            .to_string()
            .contains("重复"));
    }

    #[test]
    fn defaults_are_stable() {
        let cfg = Config::parse(r#"{ self: "a", nodes: { a: {} } }"#).unwrap();
        assert_eq!(cfg.service.dns.port, 5335);
        assert_eq!(cfg.service.wg_interface, "wg0");
        assert_eq!(cfg.service.probe_interval_ms, 3000);
        assert!(!cfg.service.wg_tunnel.enable);
        assert_eq!(cfg.service.wg_tunnel.port_base, 41000);
    }

    #[test]
    fn derives_wg_index_from_cluster_ip() {
        let cfg = Config::parse(
            r#"{ self: "a", nodes: { a: {}, b: { clusterIp: "192.168.16.13", index: 9 } } }"#,
        )
        .unwrap();
        assert_eq!(cfg.node("b").unwrap().wg_index(), Some(9));
        assert_eq!(cfg.node("a").unwrap().wg_index(), None);
        let cfg =
            Config::parse(r#"{ self: "a", nodes: { a: {}, b: { clusterIp: "192.168.16.13" } } }"#)
                .unwrap();
        assert_eq!(cfg.node("b").unwrap().wg_index(), Some(13));
    }

    #[test]
    fn wg_tunnel_requires_own_wg_port() {
        let text = r#"{
            self: "a",
            nodes: { a: { clusterIp: "192.168.16.1" }, b: {} },
            service: { wgTunnel: { enable: true } },
        }"#;
        let err = Config::parse(text).unwrap_err().to_string();
        assert!(err.contains("wgPort"), "{err}");
    }
}
