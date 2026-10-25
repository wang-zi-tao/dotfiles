//! 网络图拓扑：每个节点只发布自己实测的直连，节点之间像向量时钟一样
//! 按「信息源 + 版本号」传播、过期和合并。
//!
//! 数据模型：
//!
//! - **一个源只对自己那一端负责**：源 `s` 发布的边必须有一端是 `s`，
//!   任何节点都不能替别人背书（所以转述别人的记录也不会污染图）。
//! - **版本号**：`version` 是内容版本，只有「自己的直连结构」变化时才 +1；
//!   `generated_at` 是发布时刻，内容没变也会刷新，相当于心跳/存活证明。
//! - **按信息源管理生命周期**：每条记录记在 `sources[source]` 下，用
//!   `(version, generated_at)` 的字典序做更新判定；更旧的记录不会覆盖更新的。
//! - **过期**：`generated_at` 超过 TTL 的记录直接丢弃——源不再发布就自然消失；
//!   转述方刷新不了 `generated_at`，所以陈旧信息不会因为被反复转述而永生。
//! - **合并**：所有活着的记录（本机实测 + 各源）按边合并成图；同一对节点
//!   有多条候选时，优先本机实测，其次 Lan > Wan > Inferred，再比 rtt。

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::net::IpAddr;
use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::auth::now_unix;
use crate::config::Config;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LinkKind {
    /// 局域网直连。
    Lan,
    /// 公网直连。
    Wan,
    /// 经 cluster-node 的 h2 隧道代理的直连：对端 WireGuard 的 endpoint 指向本机隧道端口。
    Tunnel,
    /// 邻居报告的间接链路。
    Inferred,
}

impl LinkKind {
    pub fn preference(self) -> u8 {
        match self {
            LinkKind::Lan => 0,
            LinkKind::Wan => 1,
            LinkKind::Tunnel => 2,
            LinkKind::Inferred => 3,
        }
    }
}

/// 本节点到某个邻居的实测直连。
#[derive(Debug, Clone)]
pub struct Link {
    pub peer: String,
    pub address: IpAddr,
    pub port: u16,
    pub rtt_ms: f64,
    pub kind: LinkKind,
}

/// 图中的一条无向边。
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Edge {
    pub a: String,
    pub b: String,
    pub rtt_ms: f64,
    pub kind: LinkKind,
    pub updated_at: u64,
}

pub type EdgeKey = (String, String);

pub fn edge_key(a: &str, b: &str) -> EdgeKey {
    if a <= b {
        (a.to_string(), b.to_string())
    } else {
        (b.to_string(), a.to_string())
    }
}

impl Edge {
    pub fn new(a: &str, b: &str, rtt_ms: f64, kind: LinkKind) -> Self {
        Self::at(a, b, rtt_ms, kind, now_unix())
    }

    pub fn at(a: &str, b: &str, rtt_ms: f64, kind: LinkKind, updated_at: u64) -> Self {
        let (a, b) = (a.to_string(), b.to_string());
        Self {
            a,
            b,
            rtt_ms,
            kind,
            updated_at,
        }
    }

    pub fn key(&self) -> EdgeKey {
        edge_key(&self.a, &self.b)
    }
}

/// 一个信息源发布的拓扑片段（只描述以自己为一端的边）。
///
/// 这是节点之间同步的最小单位，既可以直接来自源，也可以由第三方转述；
/// 转述时 `source`/`version`/`generated_at` 原样保留。
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceRecord {
    pub source: String,
    pub version: u64,
    pub generated_at: u64,
    #[serde(default)]
    pub edges: Vec<Edge>,
}

/// 本地存下来的一份源记录。
#[derive(Debug, Clone, Default)]
pub struct StoredSource {
    pub version: u64,
    pub generated_at: u64,
    /// 本机最后一次听到这条记录的时刻（诊断用）。
    pub received_at: u64,
    pub edges: Vec<Edge>,
}

/// 合并一条记录的结果。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MergeOutcome {
    /// 新的源，第一次记录。
    Inserted,
    /// 版本更新，整条替换。
    Updated,
    /// 版本相同且发布时刻相同，只刷新接收时间（心跳）。
    Refreshed,
    /// 比手上的旧，丢弃。
    Stale,
    /// 不允许：自己发布自己的记录，或别人替别人背书。
    Rejected(&'static str),
}

/// 直连链路视图（API / CLI 输出）。
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LinkView {
    pub node: String,
    pub address: String,
    pub port: u16,
    pub rtt_ms: f64,
    pub kind: LinkKind,
}

/// 一个信息源的摘要（诊断用）。
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceView {
    pub source: String,
    pub version: u64,
    pub generated_at: u64,
    /// 距离本机几秒之前还听到过它（越大越旧）。
    pub age_secs: u64,
    pub edges: usize,
    /// 是不是本机自己。
    pub local: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TopologyView {
    pub self_node: String,
    pub version: u64,
    pub generated_at: u64,
    pub direct: Vec<LinkView>,
    pub sources: Vec<SourceView>,
    pub edges: Vec<Edge>,
}

/// 拓扑状态：本机实测 + 按信息源保存的他人记录。
#[derive(Debug, Clone)]
pub struct Topology {
    pub self_node: String,
    /// 自己实测的直连（每次探测周期性覆盖）。
    pub local: BTreeMap<String, Link>,
    /// 由 wg 隧道会话确认的直连（会话建立时登记、断开时撤销）。
    pub tunnels: BTreeMap<String, Link>,
    /// 自己那份记录的版本号与发布时刻。
    self_version: u64,
    self_generated_at: u64,
    /// 其它源发布、本机（直接或转述）收到的记录。
    pub sources: BTreeMap<String, StoredSource>,
}

impl Topology {
    pub fn new(self_node: &str) -> Self {
        Self {
            self_node: self_node.to_string(),
            local: BTreeMap::new(),
            tunnels: BTreeMap::new(),
            self_version: 0,
            self_generated_at: 0,
            sources: BTreeMap::new(),
        }
    }

    /// 用最新一轮探测结果替换直连链路，并发布自己那份记录。
    ///
    /// 只有「直连结构」（对端集合与链路类型）变化才提升版本号；rtt 抖动只刷新
    /// 发布时刻，避免版本号每轮探测都变。返回结构是否变化。
    pub fn set_local(&mut self, links: Vec<Link>) -> bool {
        let local: BTreeMap<String, Link> = links
            .into_iter()
            .map(|link| (link.peer.clone(), link))
            .collect();
        let changed = !same_structure(&self.local, &local);
        self.local = local;
        if changed || self.self_version == 0 {
            self.self_version += 1;
        }
        self.self_generated_at = now_unix();
        changed
    }

    pub fn version(&self) -> u64 {
        self.self_version
    }

    pub fn generated_at(&self) -> u64 {
        self.self_generated_at
    }

    pub fn direct(&self, node: &str) -> Option<&Link> {
        self.local.get(node).or_else(|| self.tunnels.get(node))
    }

    /// 本机直连链路：探测结果优先，其次是隧道会话确认的链路。
    ///
    /// 只能经集群地址探测到的链路（Inferred）不能盖掉隧道链路，否则明明隧道
    /// 已经跑通，却因为链路类型被排除在路由之外。
    pub fn local_links(&self) -> BTreeMap<String, Link> {
        let mut merged = self.tunnels.clone();
        for (peer, link) in &self.local {
            let hidden = merged.get(peer).is_some_and(|known| {
                known.kind == LinkKind::Tunnel && link.kind == LinkKind::Inferred
            });
            if !hidden {
                merged.insert(peer.clone(), link.clone());
            }
        }
        merged
    }

    /// 隧道会话建立时登记一条直连：对端可能在 NAT 后面，本机探测不到它的
    /// peer 端口，但隧道已经跑通了，这条链路是真实存在的。返回结构是否变化。
    pub fn set_tunnel(&mut self, peer: &str, address: IpAddr, port: u16) -> bool {
        let before = self.local_links();
        self.tunnels.insert(
            peer.to_string(),
            Link {
                peer: peer.to_string(),
                address,
                port,
                rtt_ms: 0.0,
                kind: LinkKind::Tunnel,
            },
        );
        let changed = !same_structure(&before, &self.local_links());
        if changed {
            self.self_version += 1;
        }
        self.self_generated_at = now_unix();
        changed
    }

    /// 隧道会话断开时撤销这条直连；探测到的真实链路不受影响。
    pub fn clear_tunnel(&mut self, peer: &str) -> bool {
        let before = self.local_links();
        if self.tunnels.remove(peer).is_none() {
            return false;
        }
        let changed = !same_structure(&before, &self.local_links());
        if changed {
            self.self_version += 1;
        }
        self.self_generated_at = now_unix();
        changed
    }

    /// 自己那份记录（用于同步给邻居）。
    pub fn self_record(&self) -> SourceRecord {
        SourceRecord {
            source: self.self_node.clone(),
            version: self.self_version,
            generated_at: self.self_generated_at,
            edges: self.local_edges(),
        }
    }

    /// 自己实测的边（都以自己为一端）。
    pub fn local_edges(&self) -> Vec<Edge> {
        let mut edges: Vec<Edge> = self
            .local_links()
            .values()
            .map(|link| {
                Edge::at(
                    &self.self_node,
                    &link.peer,
                    link.rtt_ms,
                    link.kind,
                    self.self_generated_at,
                )
            })
            .collect();
        edges.sort_by_key(|edge| edge.key());
        edges
    }

    /// 本机已知的全部记录（自己 + 其它源），用于同步给邻居。
    pub fn records(&self) -> Vec<SourceRecord> {
        let mut records = vec![self.self_record()];
        for (source, stored) in &self.sources {
            records.push(SourceRecord {
                source: source.clone(),
                version: stored.version,
                generated_at: stored.generated_at,
                edges: stored.edges.clone(),
            });
        }
        records
    }

    /// 按「信息源 + 版本号」合并一条记录。
    ///
    /// 关键约束：记录只能包含以 `source` 为一端的边（谁也不能替别人背书）。
    /// 记录里可以提到本机（比如对端说「我和你能直连」），这些边会原样收下并
    /// 转述，但**合并成图时不会采信**——自己是否与某节点直连只能由本机探测
    /// 决定，否则断链后邻居会一直替我们转述那条死边。
    pub fn apply_record(&mut self, record: SourceRecord) -> MergeOutcome {
        let SourceRecord {
            source,
            version,
            generated_at,
            edges,
        } = record;
        if source == self.self_node {
            return MergeOutcome::Rejected("自己发布的记录");
        }
        if version == 0 && generated_at == 0 {
            return MergeOutcome::Rejected("空记录");
        }
        let edges: Vec<Edge> = edges
            .into_iter()
            .filter(|edge| edge.a == source || edge.b == source)
            .filter(|edge| edge.a != edge.b)
            .collect();

        let now = now_unix();
        match self.sources.get_mut(&source) {
            None => {
                self.sources.insert(
                    source,
                    StoredSource {
                        version,
                        generated_at,
                        received_at: now,
                        edges,
                    },
                );
                MergeOutcome::Inserted
            }
            Some(current) => {
                let incoming = (version, generated_at);
                let known = (current.version, current.generated_at);
                match incoming.cmp(&known) {
                    Ordering::Greater => {
                        current.version = version;
                        current.generated_at = generated_at;
                        current.received_at = now;
                        current.edges = edges;
                        MergeOutcome::Updated
                    }
                    Ordering::Equal => {
                        current.received_at = now;
                        MergeOutcome::Refreshed
                    }
                    Ordering::Less => MergeOutcome::Stale,
                }
            }
        }
    }

    /// 丢弃源已经不再发布的记录（`generated_at` 超过 TTL）。
    ///
    /// 只看发布时刻而不看接收时刻：转述方每轮都会把同一条记录再发一遍，
    /// 若按接收时刻续命，源下线后它的旧信息会被永久转述。
    pub fn prune(&mut self, max_age: Duration) {
        let now = now_unix();
        let max_age = max_age.as_secs();
        self.sources
            .retain(|_, stored| now.saturating_sub(stored.generated_at) <= max_age);
    }

    /// 合并出整张图：本机实测的边优先，其余来自各信息源。
    pub fn graph_edges(&self, config: &Config) -> Vec<Edge> {
        // 边键 -> 候选（来源优先级, 链路类型, rtt）
        let mut best: BTreeMap<EdgeKey, (u8, u8, f64, Edge)> = BTreeMap::new();
        let mut consider = |edge: Edge, source_rank: u8| {
            if edge.a == edge.b || config.node(&edge.a).is_none() || config.node(&edge.b).is_none()
            {
                return;
            }
            let candidate = (source_rank, edge.kind.preference(), edge.rtt_ms);
            let key = edge.key();
            match best.get(&key) {
                Some((rank, kind, rtt, _))
                    if (candidate.0, candidate.1).cmp(&(*rank, *kind)) == Ordering::Greater
                        || ((candidate.0, candidate.1) == (*rank, *kind)
                            && candidate.2 >= *rtt) => {}
                _ => {
                    best.insert(key, (candidate.0, candidate.1, candidate.2, edge));
                }
            }
        };
        // 本机实测最可信；其它源按名字顺序（确定性）。
        for edge in self.local_edges() {
            consider(edge, 0);
        }
        for stored in self.sources.values() {
            for edge in &stored.edges {
                // 别人提到「我」的边只当作它的说法转述，不参与本机合并：
                // 自己与谁直连只能由本机探测决定。
                if edge.a == self.self_node || edge.b == self.self_node {
                    continue;
                }
                consider(edge.clone(), 1);
            }
        }
        let mut edges: Vec<Edge> = best.into_values().map(|(_, _, _, edge)| edge).collect();
        edges.sort_by_key(|edge| edge.key());
        edges
    }

    /// 已知信息源（含本机）的名字集合，用于诊断。
    pub fn known_sources(&self) -> BTreeSet<String> {
        let mut set: BTreeSet<String> = self.sources.keys().cloned().collect();
        set.insert(self.self_node.clone());
        set
    }

    pub fn view(&self) -> TopologyView {
        let now = now_unix();
        let mut direct: Vec<LinkView> = self
            .local_links()
            .values()
            .map(|link| LinkView {
                node: link.peer.clone(),
                address: link.address.to_string(),
                port: link.port,
                rtt_ms: (link.rtt_ms * 100.0).round() / 100.0,
                kind: link.kind,
            })
            .collect();
        direct.sort_by(|a, b| a.rtt_ms.total_cmp(&b.rtt_ms));

        let mut sources = vec![SourceView {
            source: self.self_node.clone(),
            version: self.self_version,
            generated_at: self.self_generated_at,
            age_secs: now.saturating_sub(self.self_generated_at),
            edges: self.local_links().len(),
            local: true,
        }];
        for (source, stored) in &self.sources {
            sources.push(SourceView {
                source: source.clone(),
                version: stored.version,
                generated_at: stored.generated_at,
                age_secs: now.saturating_sub(stored.generated_at),
                edges: stored.edges.len(),
                local: false,
            });
        }

        TopologyView {
            self_node: self.self_node.clone(),
            version: self.self_version,
            generated_at: self.self_generated_at,
            direct,
            sources,
            edges: self.graph_edges_for_view(),
        }
    }

    /// view() 里合并的边（不依赖 Config 的那部分，包含所有源；给 CLI/API 看）。
    fn graph_edges_for_view(&self) -> Vec<Edge> {
        let mut merged: BTreeMap<EdgeKey, Edge> = BTreeMap::new();
        for stored in self.sources.values() {
            for edge in &stored.edges {
                if edge.a == self.self_node || edge.b == self.self_node {
                    continue;
                }
                merged.insert(edge.key(), edge.clone());
            }
        }
        for edge in self.local_edges() {
            merged.insert(edge.key(), edge);
        }
        let mut edges: Vec<Edge> = merged.into_values().collect();
        edges.sort_by_key(|edge| edge.key());
        edges
    }
}

/// 直连结构是否相同（对端 + 链路类型）；rtt 抖动不算变化。
fn same_structure(a: &BTreeMap<String, Link>, b: &BTreeMap<String, Link>) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.iter().all(|(peer, link)| {
        b.get(peer)
            .is_some_and(|other| other.kind == link.kind && other.address == link.address)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rec(source: &str, version: u64, at: u64, edges: Vec<Edge>) -> SourceRecord {
        SourceRecord {
            source: source.to_string(),
            version,
            generated_at: at,
            edges,
        }
    }

    #[test]
    fn record_only_speaks_for_its_own_edges() {
        let mut topo = Topology::new("a");
        topo.apply_record(rec(
            "b",
            1,
            100,
            vec![
                Edge::new("b", "c", 10.0, LinkKind::Lan),
                // 没有一端是 b：伪造/越权，丢弃
                Edge::new("c", "d", 1.0, LinkKind::Wan),
                // 提到本机：原样收下并转述（这是 b 的说法），但不进本机合并图
                Edge::new("a", "b", 42.0, LinkKind::Lan),
            ],
        ));
        assert_eq!(topo.sources["b"].edges.len(), 2);
        let edges = topo.graph_edges_for_view();
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].key(), edge_key("b", "c"));
    }

    #[test]
    fn relayed_edge_to_receiver_is_not_dropped() {
        // c 收到 b 的记录，里面写着 b-c：这条边提到接收方自己，但它是 b 的
        // 直连事实，必须原样保留并继续转述，否则 a 永远学不到 b 的邻居。
        let mut topo = Topology::new("c");
        topo.apply_record(rec(
            "b",
            1,
            100,
            vec![Edge::new("b", "c", 10.0, LinkKind::Lan)],
        ));
        assert_eq!(topo.sources["b"].edges.len(), 1);
        // 但合并成图时不采信（c 自己没探测到 b 就不算有这条边）。
        assert!(topo.graph_edges_for_view().is_empty());
        // 记录会被继续转述出去（本机 + b）。
        assert_eq!(topo.records().len(), 2);
    }

    #[test]
    fn newer_version_wins_and_older_is_ignored() {
        let mut topo = Topology::new("a");
        assert_eq!(
            topo.apply_record(rec(
                "b",
                2,
                100,
                vec![Edge::new("b", "c", 10.0, LinkKind::Lan)]
            )),
            MergeOutcome::Inserted
        );
        // 旧版本（即使发布时刻更新）不能覆盖
        assert_eq!(
            topo.apply_record(rec(
                "b",
                1,
                900,
                vec![Edge::new("b", "d", 10.0, LinkKind::Lan)]
            )),
            MergeOutcome::Stale
        );
        assert_eq!(topo.sources["b"].version, 2);
        // 新版本覆盖
        assert_eq!(
            topo.apply_record(rec(
                "b",
                3,
                200,
                vec![Edge::new("b", "d", 20.0, LinkKind::Lan)]
            )),
            MergeOutcome::Updated
        );
        let edges = topo.graph_edges_for_view();
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].b, "d");
    }

    #[test]
    fn same_version_refreshes_only_receipt_time() {
        let mut topo = Topology::new("a");
        topo.apply_record(rec(
            "b",
            1,
            100,
            vec![Edge::new("b", "c", 10.0, LinkKind::Lan)],
        ));
        let first = topo.sources["b"].received_at;
        topo.sources.get_mut("b").unwrap().received_at = first.saturating_sub(5);
        assert_eq!(
            topo.apply_record(rec(
                "b",
                1,
                100,
                vec![Edge::new("b", "c", 99.0, LinkKind::Lan)]
            )),
            MergeOutcome::Refreshed
        );
        // 记录内容不变（同版本同发布时刻不覆盖）
        assert_eq!(topo.sources["b"].edges[0].rtt_ms, 10.0);
    }

    #[test]
    fn expired_source_disappears_even_if_relayed_forever() {
        let mut topo = Topology::new("a");
        topo.apply_record(rec(
            "b",
            1,
            100,
            vec![Edge::new("b", "c", 10.0, LinkKind::Lan)],
        ));
        // 记录很旧：即使刚刚才被转述过来，也应该被清掉
        topo.prune(Duration::from_secs(60));
        assert!(topo.sources.is_empty());
    }

    #[test]
    fn relay_keeps_source_identity() {
        // a 直接连不到 b，但 c 转述了 b 的记录；a 仍然知道这是 b 说的。
        let mut topo = Topology::new("a");
        topo.apply_record(rec(
            "b",
            1,
            100,
            vec![Edge::new("b", "c", 10.0, LinkKind::Lan)],
        ));
        assert_eq!(topo.records().len(), 2); // 自己 + b
        assert!(topo.records().iter().any(|r| r.source == "b"));
    }

    #[test]
    fn self_record_version_bumps_on_structural_change_only() {
        let mut topo = Topology::new("a");
        topo.set_local(vec![Link {
            peer: "b".into(),
            address: "127.0.0.1".parse().unwrap(),
            port: 1,
            rtt_ms: 10.0,
            kind: LinkKind::Lan,
        }]);
        let v1 = topo.version();
        // 只有 rtt 抖动：版本不变
        topo.set_local(vec![Link {
            peer: "b".into(),
            address: "127.0.0.1".parse().unwrap(),
            port: 1,
            rtt_ms: 20.0,
            kind: LinkKind::Lan,
        }]);
        assert_eq!(topo.version(), v1);
        // 少了一个对端：版本 +1
        topo.set_local(vec![]);
        assert_eq!(topo.version(), v1 + 1);
    }

    #[test]
    fn local_edge_wins_over_relayed_edge() {
        let mut topo = Topology::new("a");
        topo.set_local(vec![Link {
            peer: "b".into(),
            address: "127.0.0.1".parse().unwrap(),
            port: 1,
            rtt_ms: 5.0,
            kind: LinkKind::Lan,
        }]);
        topo.apply_record(rec(
            "c",
            1,
            100,
            vec![Edge::new("b", "c", 1.0, LinkKind::Lan)],
        ));
        let edges = topo.graph_edges_for_view();
        assert_eq!(edges.len(), 2);
        assert_eq!(edges[0].key(), edge_key("a", "b"));
        assert_eq!(edges[0].rtt_ms, 5.0);
    }

    #[test]
    fn tunnel_link_counts_as_direct_and_survives_inferred_probe() {
        let mut topo = Topology::new("a");
        assert!(topo.set_tunnel("b", "127.0.0.1".parse().unwrap(), 7000));
        assert!(topo
            .direct("b")
            .is_some_and(|link| link.kind == LinkKind::Tunnel));
        let v = topo.version();
        // 隧道已经跑通时，经集群地址探测到的 Inferred 不该盖掉它。
        topo.set_local(vec![Link {
            peer: "b".into(),
            address: "192.168.16.13".parse().unwrap(),
            port: 7000,
            rtt_ms: 0.0,
            kind: LinkKind::Inferred,
        }]);
        assert_eq!(topo.local_links()["b"].kind, LinkKind::Tunnel);
        assert_eq!(topo.local_edges().len(), 1);
        // 会话断开：撤销后回落到探测结果。
        assert!(topo.clear_tunnel("b"));
        assert_eq!(
            topo.direct("b").map(|link| link.kind),
            Some(LinkKind::Inferred)
        );
        assert!(topo.version() > v);
    }
}
