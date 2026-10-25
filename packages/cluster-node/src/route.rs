//! 路由计算：在实测网络上跑 Dijkstra，并生成 WireGuard 下发的
//! allowed-ips / endpoint / 路由计划。

use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result};
use petgraph::algo::astar;
use petgraph::graph::{NodeIndex, UnGraph};
use serde::{Deserialize, Serialize};

use crate::auth::now_unix;
use crate::config::Config;
use crate::topology::{LinkKind, Topology};

/// 单个目的节点的路由。
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RouteEntry {
    pub node: String,
    pub reachable: bool,
    /// 第一跳邻居。
    pub next_hop: Option<String>,
    /// 完整路径，含首尾节点。
    pub path: Vec<String>,
    /// 加权代价（毫秒，含 1 MiB 传输时间）。
    pub cost_ms: f64,
    /// 路径上的 RTT 之和。
    pub rtt_ms: f64,
    /// 第一跳直连地址。
    pub address: Option<String>,
    pub kind: Option<LinkKind>,
    pub cluster_ip: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RouteTable {
    pub self_node: String,
    pub generated_at: u64,
    pub nodes: BTreeMap<String, RouteEntry>,
}

impl RouteTable {
    pub fn get(&self, node: &str) -> Option<&RouteEntry> {
        self.nodes.get(node)
    }

    pub fn reachable(&self, node: &str) -> bool {
        self.nodes.get(node).is_some_and(|e| e.reachable)
    }
}

/// 1 MiB 数据在速度为 speed_mbps 的链路上的传输时间 + RTT。
pub fn edge_cost_ms(rtt_ms: f64, speed_mbps: f64) -> f64 {
    rtt_ms + 1000.0 / speed_mbps.max(0.001)
}

fn node_speed(config: &Config, node: &str) -> f64 {
    config.node(node).map(|n| n.speed()).unwrap_or(1000.0)
}

/// 无向边的规范化键，用于按链路查 RTT。
fn edge_key(a: &str, b: &str) -> (String, String) {
    if a <= b {
        (a.to_string(), b.to_string())
    } else {
        (b.to_string(), a.to_string())
    }
}

/// 用拓扑图计算到每个节点的最优路径。
///
/// 直连优先：有直连链路的邻居，第一跳恒为它自己。WireGuard 的 allowed-ips 是
/// 全局唯一的“目的地址 -> peer”映射，让中转节点当第一跳会把直连 peer 的 /32
/// 抢走，接收方随后按自己的 allowed-ips 校验源地址直接丢包；多跳只在没有直连
/// 链路时才用（petgraph 上的加权最短路）。
pub fn compute(config: &Config, topology: &Topology) -> RouteTable {
    // 只用真实链路（Lan/Wan）算路径：Inferred 边是本机经 WireGuard 集群地址
    // 探测到对端，它看起来「直连」，实际是穿过某个中转节点的路径。把它当成
    // 第一跳，wg_plan 会把目的节点分给它自己那个已经失效的 endpoint，永远转不出去。
    let edges: Vec<crate::topology::Edge> = topology
        .graph_edges(config)
        .into_iter()
        .filter(|edge| {
            // 只丢掉「自己」的 inferred 边：那是本机经 WireGuard 集群地址探测到的，
            // 说明它其实是通过别的节点中转过去的。邻居的 inferred 边保留，它表示的
            // 是邻居到那台机器的真实可达性，可以作为中转路径的一段。
            !(edge.kind == LinkKind::Inferred
                && (edge.a == config.self_node || edge.b == config.self_node))
        })
        .collect();

    // 建无向图：节点是集群里的主机名，边权 = 加权代价（微秒）。
    let mut graph: UnGraph<String, u64> = UnGraph::new_undirected();
    let mut index: BTreeMap<String, NodeIndex> = BTreeMap::new();
    let self_index = graph.add_node(config.self_node.clone());
    index.insert(config.self_node.clone(), self_index);
    for (name, _) in config.peers() {
        if let std::collections::btree_map::Entry::Vacant(entry) = index.entry(name.clone()) {
            entry.insert(graph.add_node(name.clone()));
        }
    }

    let mut rtt_of: BTreeMap<(String, String), f64> = BTreeMap::new();
    for edge in &edges {
        let (Some(&a), Some(&b)) = (index.get(&edge.a), index.get(&edge.b)) else {
            continue;
        };
        let speed = node_speed(config, &edge.a).min(node_speed(config, &edge.b));
        let weight = (edge_cost_ms(edge.rtt_ms, speed) * 1000.0).max(1.0) as u64;
        graph.add_edge(a, b, weight);
        rtt_of.insert(edge_key(&edge.a, &edge.b), edge.rtt_ms);
    }

    let mut nodes = BTreeMap::new();
    nodes.insert(
        config.self_node.clone(),
        RouteEntry {
            node: config.self_node.clone(),
            reachable: true,
            next_hop: None,
            path: vec![config.self_node.clone()],
            cost_ms: 0.0,
            rtt_ms: 0.0,
            address: None,
            kind: None,
            cluster_ip: config.me().cluster_ip.map(|ip| ip.to_string()),
        },
    );

    for (name, node) in config.peers() {
        let Some(&target) = index.get(name) else {
            continue;
        };
        let direct = topology
            .direct(name)
            .filter(|link| link.kind != LinkKind::Inferred);

        let (cost_micro, path): (u64, Vec<NodeIndex>) = match direct {
            // 直连优先：哪怕绕路更“便宜”，第一跳也必须是邻居自己。
            Some(link) => {
                let speed = node_speed(config, &config.self_node).min(node_speed(config, name));
                let cost = (edge_cost_ms(link.rtt_ms, speed) * 1000.0).max(1.0) as u64;
                (cost, vec![self_index, target])
            }
            None => match astar(
                &graph,
                self_index,
                |finish| finish == target,
                |edge| *edge.weight(),
                |_| 0u64,
            ) {
                Some((cost, path)) => (cost, path),
                None => {
                    nodes.insert(
                        name.clone(),
                        RouteEntry {
                            node: name.clone(),
                            reachable: false,
                            next_hop: None,
                            path: Vec::new(),
                            cost_ms: f64::INFINITY,
                            rtt_ms: f64::INFINITY,
                            address: None,
                            kind: None,
                            cluster_ip: node.cluster_ip.map(|ip| ip.to_string()),
                        },
                    );
                    continue;
                }
            },
        };

        let path_names: Vec<String> = path.iter().map(|i| graph[*i].clone()).collect();
        let rtt_ms: f64 = path_names
            .windows(2)
            .map(|pair| {
                rtt_of
                    .get(&edge_key(&pair[0], &pair[1]))
                    .copied()
                    .unwrap_or(0.0)
            })
            .sum();
        let next_hop = path_names.get(1).cloned();
        // 第一跳必须是本机实测过的直连邻居。图里若混进了别的边（例如邻居
        // 转述的旧直连边），这一跳发不出去，宁可判为不可达。
        if let Some(hop) = next_hop.as_deref() {
            if topology.direct(hop).is_none() {
                nodes.insert(
                    name.clone(),
                    RouteEntry {
                        node: name.clone(),
                        reachable: false,
                        next_hop: None,
                        path: Vec::new(),
                        cost_ms: f64::INFINITY,
                        rtt_ms: f64::INFINITY,
                        address: None,
                        kind: None,
                        cluster_ip: node.cluster_ip.map(|ip| ip.to_string()),
                    },
                );
                continue;
            }
        }
        let first_hop = next_hop.as_deref().and_then(|hop| topology.direct(hop));
        nodes.insert(
            name.clone(),
            RouteEntry {
                node: name.clone(),
                reachable: true,
                next_hop,
                path: path_names,
                cost_ms: cost_micro as f64 / 1000.0,
                rtt_ms,
                address: first_hop.map(|link| link.address.to_string()),
                kind: first_hop.map(|link| link.kind),
                cluster_ip: node.cluster_ip.map(|ip| ip.to_string()),
            },
        );
    }

    RouteTable {
        self_node: config.self_node.clone(),
        generated_at: now_unix(),
        nodes,
    }
}

/// WireGuard / 系统路由的下发计划。
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WgPlan {
    pub generated_at: u64,
    pub interface: String,
    /// 邻居 -> 该邻居承载的目的 clusterIp。
    pub allowed_ips: BTreeMap<String, Vec<String>>,
    /// 邻居 -> 直连 endpoint。
    pub endpoints: BTreeMap<String, String>,
    /// 需要确保存在的系统路由。
    pub routes: Vec<String>,
    /// 按顺序执行的命令（applyKernel=true 时真正执行）。
    pub commands: Vec<String>,
    pub warnings: Vec<String>,
}

impl WgPlan {
    pub fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }
}

/// 生成计划所需的运行时输入。
pub struct WgPlanInput<'a> {
    /// 邻居 -> 本机为它绑定的隧道端口。
    pub tunnel_ports: &'a BTreeMap<String, u16>,
    /// 已建立隧道会话的邻居（只有发起方会建会话）。
    pub sessions: &'a BTreeSet<String>,
    /// 内核里每个 peer 当前的 allowed-ips（wg pubkey -> CIDR 列表）。
    pub existing_allowed_ips: &'a BTreeMap<String, Vec<String>>,
}

/// 根据路由表生成下发给 wg0 的计划。
///
/// 目的节点按第一跳归属：直连邻居承载它自己的 clusterIp；直连断开、只能经
/// 中转节点到达时，则由第一跳承载它的 clusterIp。WireGuard 的 allowed-ips
/// 同时是「目的路由表」和「入站源地址白名单」，所以中转节点也必须把该地址
/// 分给对应的 peer，否则回程会按源地址校验被丢掉。
///
/// 内核里原有的其他网段（例如 10.16.x.0/24 这类由 Nix 配置的网段）原样保留；
/// 被错误分给别的 peer 的集群地址会被清掉，避免流量被黑洞。
///
/// endpoint 只有在"与该对端的隧道会话已经建立"时才切到 127.0.0.1:<本地端口>：
/// 发起方和响应方都要切，响应方的 WireGuard 报文也要写进自己的隧道端口，
/// 由响应流送回对端。会话没建立时保持真实地址，不打断还算能用的直连。
pub fn wg_plan(
    config: &Config,
    topology: &Topology,
    table: &RouteTable,
    input: &WgPlanInput,
) -> WgPlan {
    let interface = config.service.wg_interface.clone();
    let mut warnings = Vec::new();
    let mut routes: BTreeSet<std::net::Ipv4Addr> = BTreeSet::new();

    // 第一跳 -> 由它承载的目的 clusterIp（含它自己与经由它中转的节点）。
    let mut assigned: BTreeMap<String, BTreeSet<std::net::Ipv4Addr>> = BTreeMap::new();
    for (name, entry) in &table.nodes {
        if name == &config.self_node || !entry.reachable {
            continue;
        }
        let Some(hop) = entry.next_hop.as_deref() else {
            continue;
        };
        let Some(ip) = entry
            .cluster_ip
            .as_deref()
            .and_then(|ip| ip.parse::<std::net::Ipv4Addr>().ok())
        else {
            warnings.push(format!("节点 {name} 没有 clusterIp，无法生成 allowed-ips"));
            continue;
        };
        if topology.direct(hop).is_none() {
            warnings.push(format!("节点 {name} 的第一跳 {hop} 不是直连邻居，跳过"));
            continue;
        }
        assigned.entry(hop.to_string()).or_default().insert(ip);
        routes.insert(ip);
    }

    let cluster_ips: BTreeSet<std::net::Ipv4Addr> = config
        .nodes
        .values()
        .filter_map(|node| node.cluster_ip)
        .collect();

    // 要下发的 peer：承载了目的的邻居，加上内核里已有 allowed-ips 的邻居
    // （后者要清掉被误分给它的其他节点 clusterIp，否则这些地址会被黑洞）。
    let mut peers: BTreeSet<String> = assigned.keys().cloned().collect();
    let pubkey_to_node: BTreeMap<&str, &str> = config
        .nodes
        .iter()
        .filter_map(|(name, node)| {
            node.wg_public_key
                .as_deref()
                .map(|key| (key, name.as_str()))
        })
        .collect();
    for pubkey in input.existing_allowed_ips.keys() {
        if let Some(name) = pubkey_to_node.get(pubkey.as_str()) {
            peers.insert((*name).to_string());
        }
    }

    let mut allowed: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut commands = Vec::new();
    for peer in &peers {
        let Some(node) = config.node(peer) else {
            continue;
        };
        let Some(pubkey) = node.wg_public_key.as_deref() else {
            if assigned.contains_key(peer) {
                warnings.push(format!("邻居 {peer} 没有 wgPublicKey，跳过 wg set"));
            }
            continue;
        };
        let existing: BTreeSet<String> = input
            .existing_allowed_ips
            .get(pubkey)
            .map(|ips| ips.iter().cloned().collect())
            .unwrap_or_default();
        // 保留非集群地址（10.16.x.0/24 之类），清掉被错误分给本 peer 的
        // 其他节点 clusterIp。
        let mut wanted: BTreeSet<String> = existing
            .iter()
            .filter(|cidr| !is_cluster_address(cidr, &cluster_ips))
            .cloned()
            .collect();
        if let Some(ips) = assigned.get(peer) {
            for ip in ips {
                wanted.insert(format!("{ip}/32"));
            }
            // 内核里可能已经被旧实现把 Nix 配置的网段清掉了（例如 10.16.x.0/24），
            // 用配置里的 clusterIpRange 兜底恢复。
            if let Some(range) = node.cluster_ip_range.as_deref() {
                wanted.insert(range.to_string());
            }
        }
        if wanted != existing {
            let list = wanted.iter().cloned().collect::<Vec<_>>().join(",");
            commands.push(format!(
                "wg set {interface} peer {pubkey} allowed-ips {list}"
            ));
        }
        if let Some(ips) = assigned.get(peer) {
            allowed.insert(peer.clone(), ips.iter().map(|ip| ip.to_string()).collect());
        }
    }

    let mut endpoints = BTreeMap::new();
    for (peer, link) in topology.local_links().iter() {
        let Some(port) = config.node(peer).and_then(|node| node.wg_port) else {
            continue;
        };
        let proxy = crate::wgtunnel::should_proxy(&config.service.wg_tunnel, link.kind)
            && input.sessions.contains(peer);
        let endpoint = match input.tunnel_ports.get(peer) {
            Some(local_port) if proxy => format!("127.0.0.1:{local_port}"),
            _ => format!("{}:{port}", link.address),
        };
        endpoints.insert(peer.clone(), endpoint);
    }

    if config.service.manage_endpoints {
        for peer in &peers {
            let (Some(node), Some(endpoint)) = (config.node(peer), endpoints.get(peer)) else {
                continue;
            };
            let Some(pubkey) = node.wg_public_key.as_deref() else {
                continue;
            };
            commands.push(format!(
                "wg set {interface} peer {pubkey} endpoint {endpoint} persistent-keepalive 32"
            ));
        }
    }
    for ip in &routes {
        commands.push(format!("ip route replace {ip}/32 dev {interface}"));
    }

    if assigned.is_empty() {
        warnings.push("没有可下发的邻居（可能全部不可达）".to_string());
    }

    WgPlan {
        generated_at: now_unix(),
        interface,
        allowed_ips: allowed,
        endpoints,
        routes: routes.iter().map(|ip| format!("{ip}/32")).collect(),
        commands,
        warnings,
    }
}

/// CIDR 是否指向某个集群节点的 clusterIp。
fn is_cluster_address(cidr: &str, cluster_ips: &BTreeSet<std::net::Ipv4Addr>) -> bool {
    let addr = cidr.split('/').next().unwrap_or(cidr);
    addr.parse::<std::net::Ipv4Addr>()
        .map(|ip| cluster_ips.contains(&ip))
        .unwrap_or(false)
}

/// 读取内核里 wg 接口当前每个 peer 的 allowed-ips。
pub async fn current_allowed_ips(interface: &str) -> BTreeMap<String, Vec<String>> {
    let output = tokio::process::Command::new("wg")
        .args(["show", interface, "allowed-ips"])
        .output()
        .await;
    let Ok(output) = output else {
        return BTreeMap::new();
    };
    if !output.status.success() {
        return BTreeMap::new();
    }
    let text = String::from_utf8_lossy(&output.stdout);
    let mut map: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for line in text.lines() {
        let mut parts = line.split('\t');
        let Some(pubkey) = parts.next().map(str::trim).filter(|key| !key.is_empty()) else {
            continue;
        };
        let ips = parts
            .next()
            .unwrap_or("")
            .split_whitespace()
            .filter(|ip| *ip != "(none)")
            .map(str::to_string)
            .collect::<Vec<_>>();
        map.insert(pubkey.to_string(), ips);
    }
    map
}
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CommandReport {
    pub command: String,
    pub ok: bool,
    pub stderr: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ApplyReport {
    pub applied: bool,
    pub plan_path: String,
    pub commands: Vec<CommandReport>,
    pub warnings: Vec<String>,
}

/// 落盘计划；applyKernel=true 且 run_commands 时才真正执行命令。
///
/// run_commands=false 用于命令没有变化时跳过重复下发（endpoint 切换之类
/// 的操作没必要每 5 秒重做一遍）。
pub async fn apply(plan: &WgPlan, config: &Config, run_commands: bool) -> Result<ApplyReport> {
    let state_dir = &config.service.state_dir;
    tokio::fs::create_dir_all(state_dir)
        .await
        .with_context(|| format!("创建状态目录 {} 失败", state_dir.display()))?;
    let plan_path = state_dir.join("wg-plan.json");
    tokio::fs::write(&plan_path, serde_json::to_vec_pretty(plan)?)
        .await
        .with_context(|| format!("写入 {} 失败", plan_path.display()))?;

    let mut report = ApplyReport {
        applied: config.service.apply_kernel && run_commands,
        plan_path: plan_path.display().to_string(),
        commands: Vec::new(),
        warnings: plan.warnings.clone(),
    };
    if !report.applied {
        return Ok(report);
    }

    for command in &plan.commands {
        let output = tokio::process::Command::new("sh")
            .arg("-c")
            .arg(command)
            .output()
            .await;
        match output {
            Ok(output) => report.commands.push(CommandReport {
                command: command.clone(),
                ok: output.status.success(),
                stderr: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            }),
            Err(err) => report.commands.push(CommandReport {
                command: command.clone(),
                ok: false,
                stderr: err.to_string(),
            }),
        }
    }
    Ok(report)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::topology::Edge;
    use crate::topology::Link;

    fn config() -> Config {
        Config::parse(
            r#"{
                self: "a",
                nodes: {
                    a: { localIp: "10.0.0.1", clusterIp: "192.168.16.1", wgPublicKey: "pk-a", speed: 1000 },
                    b: { localIp: "10.0.0.2", clusterIp: "192.168.16.2", wgPublicKey: "pk-b", speed: 1000 },
                    c: { localIp: "10.0.0.3", clusterIp: "192.168.16.3", wgPublicKey: "pk-c", speed: 1000 },
                },
            }"#,
        )
        .unwrap()
    }

    fn link(peer: &str, rtt: f64) -> Link {
        Link {
            peer: peer.to_string(),
            address: format!("10.0.0.{}", if peer == "b" { 2 } else { 3 })
                .parse()
                .unwrap(),
            port: 7788,
            rtt_ms: rtt,
            kind: LinkKind::Lan,
        }
    }

    /// 模拟一次 gossip：把 source 自己那份记录（版本号递增）合并进来。
    fn gossip(topo: &mut Topology, source: &str, edges: Vec<Edge>) {
        let version = topo
            .sources
            .get(source)
            .map_or(1, |stored| stored.version + 1);
        topo.apply_record(crate::topology::SourceRecord {
            source: source.to_string(),
            version,
            generated_at: crate::auth::now_unix(),
            edges,
        });
    }

    #[test]
    fn picks_transit_path_and_next_hop() {
        let cfg = config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        gossip(
            &mut topo,
            "b",
            vec![
                Edge::new("b", "c", 4.0, LinkKind::Lan),
                Edge::new("a", "b", 5.0, LinkKind::Lan),
            ],
        );
        let table = compute(&cfg, &topo);
        let c = &table.nodes["c"];
        assert!(c.reachable);
        assert_eq!(c.path, vec!["a", "b", "c"]);
        assert_eq!(c.next_hop.as_deref(), Some("b"));
        assert!((c.rtt_ms - 9.0).abs() < 0.001, "{}", c.rtt_ms);
        assert!(!table.nodes["a"].reachable || table.nodes["a"].next_hop.is_none());
    }

    #[test]
    fn prefers_faster_weighted_path() {
        let cfg = Config::parse(
            r#"{
                self: "a",
                nodes: {
                    a: { speed: 1000 },
                    b: { speed: 1 },
                    c: { speed: 1000 },
                    d: { speed: 1000 },
                },
            }"#,
        )
        .unwrap();
        let mut topo = Topology::new("a");
        // b 很快但链路速度慢；c 稍慢一点但链路快。
        topo.set_local(vec![link("b", 1.0), link("c", 20.0)]);
        gossip(
            &mut topo,
            "b",
            vec![
                Edge::new("a", "b", 1.0, LinkKind::Lan),
                Edge::new("b", "d", 1.0, LinkKind::Lan),
            ],
        );
        gossip(
            &mut topo,
            "c",
            vec![
                Edge::new("a", "c", 20.0, LinkKind::Lan),
                Edge::new("c", "d", 20.0, LinkKind::Lan),
            ],
        );
        let table = compute(&cfg, &topo);
        assert_eq!(table.nodes["d"].next_hop.as_deref(), Some("c"));
    }

    #[test]
    fn unreachable_nodes_are_marked() {
        let cfg = config();
        let topo = Topology::new("a");
        let table = compute(&cfg, &topo);
        assert!(!table.nodes["b"].reachable);
        assert!(table.nodes["b"].path.is_empty());
    }

    fn plan_input<'a>(
        ports: &'a BTreeMap<String, u16>,
        sessions: &'a BTreeSet<String>,
        existing: &'a BTreeMap<String, Vec<String>>,
    ) -> WgPlanInput<'a> {
        WgPlanInput {
            tunnel_ports: ports,
            sessions,
            existing_allowed_ips: existing,
        }
    }

    #[test]
    fn gossiped_edge_touching_self_is_not_a_first_hop() {
        // a 只能直连 c；b 通过 c 才可达。但邻居仍然转述一条旧的 a-b 直连边
        // （带宽 rtt 很低），compute 不能把 b 的第一跳定为它自己。
        let cfg = config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("c", 88.0)]);
        gossip(
            &mut topo,
            "c",
            vec![
                Edge::new("a", "b", 42.0, LinkKind::Lan),
                Edge::new("b", "c", 90.0, LinkKind::Wan),
            ],
        );
        let table = compute(&cfg, &topo);
        let b = &table.nodes["b"];
        assert!(b.reachable, "b 应经 c 可达");
        assert_eq!(b.next_hop.as_deref(), Some("c"));
        assert_eq!(
            b.path,
            vec!["a".to_string(), "c".to_string(), "b".to_string()]
        );
    }

    #[test]
    fn wg_plan_assigns_transit_node_to_first_hop() {
        let cfg = config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        // b 与 c 直连，a 只能经 b 到 c：allowed-ips 里 c 的 clusterIp 必须
        // 分给第一跳 b，否则 a 永远转不到 c。
        gossip(
            &mut topo,
            "b",
            vec![
                Edge::new("a", "b", 5.0, LinkKind::Lan),
                Edge::new("b", "c", 4.0, LinkKind::Lan),
            ],
        );
        let table = compute(&cfg, &topo);
        assert_eq!(table.nodes["c"].next_hop.as_deref(), Some("b"));
        let ports = BTreeMap::new();
        let sessions = BTreeSet::new();
        let existing = BTreeMap::new();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert_eq!(
            plan.allowed_ips["b"],
            vec!["192.168.16.2".to_string(), "192.168.16.3".to_string()]
        );
        assert!(plan.commands.iter().any(
            |c| c.contains("wg set wg0 peer pk-b allowed-ips 192.168.16.2/32,192.168.16.3/32")
        ));
        assert!(plan
            .commands
            .iter()
            .any(|c| c.contains("ip route replace 192.168.16.3/32")));
        // 直连 peer b 自己仍然只承载自己的地址 + c 转过来的地址，不重复。
        assert_eq!(plan.allowed_ips.len(), 1);
    }

    #[test]
    fn wg_plan_clears_stale_cluster_ip_from_non_direct_peer() {
        let cfg = config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        gossip(
            &mut topo,
            "b",
            vec![Edge::new("b", "c", 4.0, LinkKind::Lan)],
        );
        let table = compute(&cfg, &topo);
        // 内核里 c 的 peer 还留着旧实现塞进去的 192.168.16.3/32，但 c 现在
        // 不是直连邻居：必须清掉，否则发往 c 的包会被送进死 endpoint。
        let existing = BTreeMap::from([(
            "pk-c".to_string(),
            vec!["192.168.16.3/32".to_string(), "10.16.3.0/24".to_string()],
        )]);
        let ports = BTreeMap::new();
        let sessions = BTreeSet::new();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert!(plan
            .commands
            .iter()
            .any(|c| c.contains("wg set wg0 peer pk-c allowed-ips 10.16.3.0/24")));
        assert!(!plan
            .commands
            .iter()
            .any(|c| c.contains("peer pk-c allowed-ips 10.16.3.0/24,192.168.16.3/32")));
    }

    #[test]
    fn remote_inferred_edge_still_works_as_transit() {
        // 邻居 gz 经集群地址探测到 asus（Inferred），说明 gz 能到 asus。
        // 自己（nuc）没有到 asus 的直连，应该走 nuc -> gz -> asus。
        let cfg = config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 88.0)]);
        gossip(
            &mut topo,
            "b",
            vec![Edge::new("b", "c", 90.0, LinkKind::Inferred)],
        );
        let table = compute(&cfg, &topo);
        let c = &table.nodes["c"];
        assert!(c.reachable, "c 应经 b 可达");
        assert_eq!(c.next_hop.as_deref(), Some("b"));
    }

    #[test]
    fn inferred_only_neighbor_is_not_routable() {
        let cfg = config();
        let mut topo = Topology::new("a");
        let mut inferred = link("b", 5.0);
        inferred.kind = LinkKind::Inferred;
        topo.set_local(vec![inferred]);
        let table = compute(&cfg, &topo);
        // 只能经集群地址探测到的邻居，实际是穿过中转节点的路径，不能当成第一跳。
        assert!(!table.nodes["b"].reachable);
    }

    #[test]
    fn wg_plan_keeps_extra_subnets_and_drops_stolen_cluster_ips() {
        let cfg = config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        let table = compute(&cfg, &topo);
        let existing = BTreeMap::from([(
            "pk-b".to_string(),
            vec![
                "192.168.16.2/32".to_string(),
                "192.168.16.3/32".to_string(),
                "10.16.2.0/24".to_string(),
            ],
        )]);
        let ports = BTreeMap::new();
        let sessions = BTreeSet::new();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert!(plan
            .commands
            .iter()
            .any(|c| c.contains("allowed-ips 10.16.2.0/24,192.168.16.2/32")));
    }

    #[test]
    fn wg_plan_is_quiet_when_allowed_ips_unchanged() {
        let cfg = config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        let table = compute(&cfg, &topo);
        let existing = BTreeMap::from([("pk-b".to_string(), vec!["192.168.16.2/32".to_string()])]);
        let ports = BTreeMap::new();
        let sessions = BTreeSet::new();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert!(!plan.commands.iter().any(|c| c.contains("allowed-ips")));
    }

    fn wg_config() -> Config {
        Config::parse(
            r#"{
                self: "a",
                nodes: {
                    a: { clusterIp: "192.168.16.12", wgPort: 16538, wgPublicKey: "pk-a" },
                    b: { clusterIp: "192.168.16.13", wgPort: 16538, wgPublicKey: "pk-b" },
                },
                service: {
                    wgTunnel: { enable: true, proxyLan: true },
                    manageEndpoints: true
                },
            }"#,
        )
        .unwrap()
    }

    fn no_existing() -> BTreeMap<String, Vec<String>> {
        BTreeMap::new()
    }

    #[test]
    fn tunnel_session_alone_makes_peer_routable() {
        // 对端在 NAT 后、本机探测不到它的 peer 端口：只靠隧道会话确认的直连，
        // 也必须分配它的 clusterIp 并把 endpoint 切到本机隧道端口。
        let cfg = wg_config();
        let mut topo = Topology::new("a");
        assert!(topo.set_tunnel("b", "127.0.0.1".parse().unwrap(), 7788));
        let table = compute(&cfg, &topo);
        assert!(table.nodes["b"].reachable);
        assert_eq!(table.nodes["b"].next_hop.as_deref(), Some("b"));
        let ports = BTreeMap::from([("b".to_string(), 41013u16)]);
        let sessions = BTreeSet::from(["b".to_string()]);
        let existing = no_existing();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert_eq!(plan.endpoints["b"], "127.0.0.1:41013");
        assert!(plan
            .commands
            .iter()
            .any(|c| c.contains("allowed-ips 192.168.16.13/32")));
    }

    #[test]
    fn direct_neighbor_endpoint_points_to_tunnel() {
        let cfg = wg_config();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        let table = compute(&cfg, &topo);
        let ports = BTreeMap::from([("b".to_string(), 41013u16)]);
        let sessions = BTreeSet::from(["b".to_string()]);
        let existing = no_existing();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert_eq!(plan.endpoints["b"], "127.0.0.1:41013");
        assert!(plan
            .commands
            .iter()
            .any(|c| c.contains("endpoint 127.0.0.1:41013")));
    }

    #[test]
    fn wan_is_proxied_by_default_but_lan_is_not() {
        let cfg = wg_config();
        let ports = BTreeMap::from([("b".to_string(), 41013u16)]);
        let sessions = BTreeSet::from(["b".to_string()]);
        let existing = no_existing();

        // 跨网段（Wan）：默认走隧道。
        let mut wan = link("b", 5.0);
        wan.kind = LinkKind::Wan;
        let mut topo = Topology::new("a");
        topo.set_local(vec![wan]);
        let table = compute(&cfg, &topo);
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert_eq!(plan.endpoints["b"], "127.0.0.1:41013");

        // 同网段（Lan）：proxyLan 关闭时保持直连。
        let mut cfg = wg_config();
        cfg.service.wg_tunnel.proxy_lan = false;
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        let table = compute(&cfg, &topo);
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert_eq!(plan.endpoints["b"], "10.0.0.2:16538");
    }

    #[test]
    fn inferred_neighbor_keeps_real_endpoint() {
        let cfg = wg_config();
        let mut topo = Topology::new("a");
        let mut inferred = link("b", 5.0);
        inferred.kind = LinkKind::Inferred;
        topo.set_local(vec![inferred]);
        let table = compute(&cfg, &topo);
        let ports = BTreeMap::from([("b".to_string(), 41013u16)]);
        let sessions = BTreeSet::from(["b".to_string()]);
        let existing = no_existing();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        // 不可路由（不能当成第一跳），但 endpoint 仍保留真实地址，不切隧道。
        assert!(!table.nodes["b"].reachable);
        assert_eq!(plan.endpoints["b"], "10.0.0.2:16538");
    }

    #[test]
    fn endpoint_stays_real_until_tunnel_session_is_up() {
        let cfg = wg_config();
        let mut wan = link("b", 5.0);
        wan.kind = LinkKind::Wan;
        let mut topo = Topology::new("a");
        topo.set_local(vec![wan]);
        let table = compute(&cfg, &topo);
        let ports = BTreeMap::from([("b".to_string(), 41013u16)]);
        let sessions = BTreeSet::new();
        let existing = no_existing();
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert_eq!(plan.endpoints["b"], "10.0.0.2:16538");
    }
    #[test]
    fn wg_plan_restores_configured_range_when_kernel_lost_it() {
        let cfg = Config::parse(
            r#"{
                self: "a",
                nodes: {
                    a: { clusterIp: "192.168.16.12", wgPublicKey: "pk-a" },
                    b: {
                        clusterIp: "192.168.16.13",
                        wgPublicKey: "pk-b",
                        clusterIpRange: "10.16.13.0/24"
                    },
                },
            }"#,
        )
        .unwrap();
        let mut topo = Topology::new("a");
        topo.set_local(vec![link("b", 5.0)]);
        let table = compute(&cfg, &topo);
        let ports = BTreeMap::new();
        let sessions = BTreeSet::new();
        // 内核里该 peer 的 allowed-ips 已被旧实现清空，配置里的网段要补回来。
        let existing = BTreeMap::from([("pk-b".to_string(), Vec::new())]);
        let plan = wg_plan(
            &cfg,
            &topo,
            &table,
            &plan_input(&ports, &sessions, &existing),
        );
        assert!(plan
            .commands
            .iter()
            .any(|c| c.contains("allowed-ips 10.16.13.0/24,192.168.16.13/32")));
    }
    #[test]
    fn direct_link_wins_even_if_detour_is_cheaper() {
        let cfg = config();
        let mut topo = Topology::new("a");
        // a-b 直连很慢（400ms），绕 a -> c -> b 只要 2ms；但 allowed-ips 的归属
        // 要求第一跳必须是直连的 b 自己，所以路由表也得保留直连。
        topo.set_local(vec![link("b", 400.0), link("c", 1.0)]);
        gossip(
            &mut topo,
            "c",
            vec![Edge::new("c", "b", 1.0, LinkKind::Lan)],
        );
        let table = compute(&cfg, &topo);
        assert_eq!(table.nodes["b"].next_hop.as_deref(), Some("b"));
        assert_eq!(
            table.nodes["b"].path,
            vec!["a".to_string(), "b".to_string()]
        );
        assert!(table.nodes["b"].cost_ms > 300.0);
        assert_eq!(table.nodes["c"].next_hop.as_deref(), Some("c"));
    }
}
