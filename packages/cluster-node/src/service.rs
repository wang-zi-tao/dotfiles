//! axum 服务：对等节点 HTTP/2 API、本地 unix socket API 与后台任务。
//!
//! 对等 API 全部要求 SSH host key 签名（auth.rs），TLS 只提供加密；
//! 本地 unix socket 只允许同机访问，因此不做请求签名。

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::net::{IpAddr, SocketAddr};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result};
use axum::body::Body;
use axum::extract::{Query, Request, State};
use axum::http::StatusCode;
use axum::middleware::{self, Next};
use axum::response::Response;
use axum::routing::{get, post};
use axum::{Json, Router};
use bytes::Bytes;
use futures_util::stream::{self, StreamExt};
use hyper_util::rt::{TokioExecutor, TokioIo};
use hyper_util::service::TowerToHyperService;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use tokio::net::{TcpListener, UdpSocket, UnixListener};
use tokio::sync::{mpsc, oneshot, Mutex, RwLock};
use tokio_rustls::TlsAcceptor;
use tracing::{debug, info, warn};

use crate::auth::{self, AuthHeaders, Identity, ReplayCache};
use crate::config::{Config, Tunnel};
use crate::dns::{self, DnsService};
use crate::framing::{encode_frame, FrameReader};
use crate::netwatch;
use crate::peer::PeerClient;
use crate::probe;
use crate::route::{self, ApplyReport, RouteTable};
use crate::topology::{Edge, LinkView, MergeOutcome, SourceRecord, SourceView, Topology};
use crate::tunnel;
use crate::wgtunnel::{self, WgTunnelSet};

#[derive(Clone)]
pub struct AppState {
    pub config: Arc<Config>,
    pub identity: Arc<Identity>,
    pub topology: Arc<RwLock<Topology>>,
    pub routes: Arc<RwLock<RouteTable>>,
    pub peers: PeerClient,
    pub started_at: u64,
    pub replay: Arc<Mutex<ReplayCache>>,
    pub last_probe_at: Arc<RwLock<Option<u64>>>,
    pub last_apply: Arc<RwLock<Option<ApplyReport>>>,
    /// 已经提示过“只能经 WireGuard 集群地址可达”的节点，避免每轮探测刷日志。
    pub inferred_nodes: Arc<Mutex<BTreeSet<String>>>,
    /// WireGuard 自动隧道的本地端口集合，未启用时为 None。
    pub wg: Option<Arc<WgTunnelSet>>,
    /// 当前已经建立的 WireGuard 隧道会话（对端节点名）。
    pub wg_sessions: Arc<Mutex<BTreeSet<String>>>,
    /// 上一次真正执行过的 wg/ip 命令，用来避免重复执行。
    pub last_commands: Arc<Mutex<Vec<String>>>,
    /// 网络事件触发过的重新探测次数。
    pub net_events: Arc<AtomicU64>,
    /// 最近一次触发重新探测的网络变化。
    pub last_net_event: Arc<RwLock<Option<NetEventInfo>>>,
}

impl AppState {
    pub fn new(config: Arc<Config>, identity: Arc<Identity>) -> Result<Self> {
        let peers = PeerClient::new(Duration::from_millis(
            config.service.probe_timeout_ms.max(100),
        ))
        .context("初始化 HTTP/2 客户端失败")?;
        let topology = Topology::new(&config.self_node);
        let routes = route::compute(&config, &topology);
        let replay = ReplayCache::new(Duration::from_secs(
            config.service.auth_skew_secs.saturating_mul(2).max(60),
        ));
        let wg = if config.service.wg_tunnel.enable {
            match WgTunnelSet::bind(&config) {
                Ok(wg) => Some(Arc::new(wg)),
                Err(err) => {
                    warn!(error = %err, "初始化 WireGuard 隧道失败，继续直连");
                    None
                }
            }
        } else {
            None
        };

        Ok(Self {
            config,
            identity,
            topology: Arc::new(RwLock::new(topology)),
            routes: Arc::new(RwLock::new(routes)),
            peers,
            started_at: auth::now_unix(),
            replay: Arc::new(Mutex::new(replay)),
            last_probe_at: Arc::new(RwLock::new(None)),
            last_apply: Arc::new(RwLock::new(None)),
            inferred_nodes: Arc::new(Mutex::new(BTreeSet::new())),
            wg,
            wg_sessions: Arc::new(Mutex::new(BTreeSet::new())),
            last_commands: Arc::new(Mutex::new(Vec::new())),
            net_events: Arc::new(AtomicU64::new(0)),
            last_net_event: Arc::new(RwLock::new(None)),
        })
    }

    fn probe_timeout(&self) -> Duration {
        Duration::from_millis(self.config.service.probe_timeout_ms.max(100))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GraphResponse {
    pub self_node: String,
    pub generated_at: u64,
    pub version: u64,
    /// 各个信息源的记录（含自己）。转述时原样保留来源与版本号，接收方据此判断新鲜度。
    #[serde(default)]
    pub sources: Vec<SourceRecord>,
    /// 合并后的整张图（便于直接查看，同步以 sources 为准）。
    #[serde(default)]
    pub edges: Vec<Edge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InfoResponse {
    pub node: String,
    pub started_at: u64,
    pub host_key: String,
    pub host_key_fingerprint: String,
    pub generated_key: bool,
    pub peer_port: u16,
    pub version: String,
}

/// 最近一次触发重新探测的网络变化。
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NetEventInfo {
    /// 中文描述，例如“接口 wlp132s0 变为 UP”。
    pub change: String,
    /// 触发时间（unix 秒）。
    pub at: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StatusResponse {
    pub node: String,
    pub started_at: u64,
    pub host_key: String,
    pub host_key_fingerprint: String,
    pub generated_key: bool,
    pub direct: Vec<LinkView>,
    /// 已收到的信息源记录摘要（含自己）。
    pub sources: Vec<SourceView>,
    pub routes: RouteTable,
    pub last_probe_at: Option<u64>,
    pub last_apply: Option<ApplyReport>,
    pub wg_tunnels: Vec<wgtunnel::ChannelView>,
    /// 网络事件触发过多少次重新探测。
    pub net_events: u64,
    /// 最近一次触发重新探测的网络变化。
    pub last_net_event: Option<NetEventInfo>,
}

fn version() -> String {
    option_env!("CLUSTER_NODE_VERSION")
        .unwrap_or(env!("CARGO_PKG_VERSION"))
        .to_string()
}

// ---------------------------------------------------------------- 认证中间件

async fn auth_layer(
    State(state): State<AppState>,
    req: Request,
    next: Next,
) -> Result<Response, StatusCode> {
    let headers = req.headers();
    let get = |name: &str| {
        headers
            .get(name)
            .and_then(|value| value.to_str().ok())
            .map(str::to_string)
    };
    let (Some(node), Some(time), Some(nonce), Some(signature)) = (
        get("x-cluster-node"),
        get("x-cluster-time"),
        get("x-cluster-nonce"),
        get("x-cluster-signature"),
    ) else {
        return Err(StatusCode::UNAUTHORIZED);
    };
    let Some(peer) = state.config.node(&node) else {
        return Err(StatusCode::FORBIDDEN);
    };
    if peer.host_pub_key.trim().is_empty() {
        return Err(StatusCode::FORBIDDEN);
    }
    let Ok(auth_headers) = AuthHeaders::from_parts(node, time, nonce, signature) else {
        return Err(StatusCode::UNAUTHORIZED);
    };
    let method = req.method().as_str().to_string();
    let path = req.uri().path().to_string();
    let skew = state.config.service.auth_skew_secs;
    let mut replay = state.replay.lock().await;
    if let Err(err) = auth::verify(
        &peer.host_pub_key,
        &auth_headers,
        &method,
        &path,
        skew,
        &mut replay,
    ) {
        debug!(error = %err, "拒绝未通过认证的请求");
        return Err(StatusCode::UNAUTHORIZED);
    }
    drop(replay);
    Ok(next.run(req).await)
}

// ------------------------------------------------------------------ handlers

async fn ping(State(state): State<AppState>) -> Json<Value> {
    Json(json!({
        "node": state.config.self_node,
        "time": auth::now_unix(),
    }))
}

async fn info(State(state): State<AppState>) -> Json<InfoResponse> {
    Json(InfoResponse {
        node: state.config.self_node.clone(),
        started_at: state.started_at,
        host_key: state.identity.public_openssh.clone(),
        host_key_fingerprint: state.identity.fingerprint(),
        generated_key: state.identity.generated,
        peer_port: state.config.me().peer_port(&state.config.service),
        version: version(),
    })
}

async fn graph(State(state): State<AppState>) -> Json<GraphResponse> {
    let topo = state.topology.read().await;
    Json(graph_response(&state.config, &topo))
}

/// 构造同步给邻居的图：自己的记录 + 已收到的其它源记录 + 合并后的边。
fn graph_response(config: &Config, topo: &Topology) -> GraphResponse {
    GraphResponse {
        self_node: topo.self_node.clone(),
        generated_at: topo.generated_at(),
        version: topo.version(),
        sources: topo.records(),
        edges: topo.graph_edges(config),
    }
}

/// 源记录的存活时间：至少 60 秒，默认是 gossip 周期的 6 倍。
fn gossip_ttl(config: &Config) -> Duration {
    Duration::from_millis(config.service.gossip_interval_ms.saturating_mul(6))
        .max(Duration::from_secs(60))
}

/// 记录是否已经超过存活时间（generated_at 太旧）。
///
/// 必须在合并前判断：否则本机刚 prune 掉的过期记录，会被下一轮 gossip 从
/// 邻居那里原样搬回来（邻居还没 prune），形成「永远转述」。
fn record_expired(record: &SourceRecord, now: u64, ttl: Duration) -> bool {
    now.saturating_sub(record.generated_at) > ttl.as_secs()
}

/// 把一份 /v1/graph 响应按「信息源 + 版本号」合并进本地拓扑。
///
/// 记录可以来自源本身，也可以由对方转述：转述保留原始 source/version，
/// 所以本机连不到某个节点时，也能从别的节点那里拿到它的直连信息。
/// 返回 (接受条数, 丢弃条数)。
async fn merge_graph(state: &AppState, from: &str, graph: GraphResponse) -> (usize, usize) {
    let records = if graph.sources.is_empty() {
        // 旧版本节点只返回合并后的边：把它当作该节点自己那份记录（版本从 0 开始）。
        vec![SourceRecord {
            source: graph.self_node.clone(),
            version: 0,
            generated_at: graph.generated_at,
            edges: graph.edges,
        }]
    } else {
        graph.sources
    };
    let mut topo = state.topology.write().await;
    let ttl = gossip_ttl(&state.config);
    let now = auth::now_unix();
    let mut accepted = 0usize;
    let mut rejected = 0usize;
    for record in records {
        if record_expired(&record, now, ttl) {
            debug!(
                node = %from,
                source = %record.source,
                age = now.saturating_sub(record.generated_at),
                "丢弃过期的拓扑记录"
            );
            rejected += 1;
            continue;
        }
        let source = record.source.clone();
        match topo.apply_record(record) {
            MergeOutcome::Inserted | MergeOutcome::Updated | MergeOutcome::Refreshed => {
                accepted += 1
            }
            MergeOutcome::Stale => rejected += 1,
            MergeOutcome::Rejected(reason) => {
                debug!(node = %from, source = %source, reason, "丢弃拓扑记录");
                rejected += 1;
            }
        }
    }
    (accepted, rejected)
}

async fn routes_handler(State(state): State<AppState>) -> Json<RouteTable> {
    Json(state.routes.read().await.clone())
}

async fn status(State(state): State<AppState>) -> Json<StatusResponse> {
    let view = state.topology.read().await.view();
    let direct = view.direct;
    let wg_tunnels = match &state.wg {
        Some(wg) => {
            let active = state.wg_sessions.lock().await;
            wg.views(&active)
        }
        None => Vec::new(),
    };
    Json(StatusResponse {
        node: state.config.self_node.clone(),
        started_at: state.started_at,
        host_key: state.identity.public_openssh.clone(),
        host_key_fingerprint: state.identity.fingerprint(),
        generated_key: state.identity.generated,
        direct,
        sources: view.sources,
        routes: state.routes.read().await.clone(),
        last_probe_at: *state.last_probe_at.read().await,
        last_apply: state.last_apply.read().await.clone(),
        wg_tunnels,
        net_events: state.net_events.load(Ordering::Relaxed),
        last_net_event: state.last_net_event.read().await.clone(),
    })
}

async fn rescan(State(state): State<AppState>) -> Json<Value> {
    let summary = refresh(&state).await;
    Json(json!({
        "ok": summary.links.is_ok(),
        "direct": summary.direct,
        "edges": summary.edges,
        "error": summary.links.err(),
    }))
}

#[derive(Debug, Deserialize)]
struct TunnelQuery {
    target: String,
}

async fn tunnel_handler(
    State(state): State<AppState>,
    Query(query): Query<TunnelQuery>,
    body: Body,
) -> Result<Response, (StatusCode, String)> {
    let target: SocketAddr = query.target.parse().map_err(|_| {
        (
            StatusCode::BAD_REQUEST,
            format!("target 不是合法的 ip:port：{}", query.target),
        )
    })?;
    if !probe::target_allowed(&state.config, target.ip()) {
        return Err((
            StatusCode::FORBIDDEN,
            format!("目标 {target} 不在允许列表内"),
        ));
    }
    tunnel::proxy(target, body, Duration::from_secs(60))
        .await
        .map_err(|err| (StatusCode::BAD_GATEWAY, err.to_string()))
}

// ------------------------------------------------------ WireGuard 自动隧道

/// 接收侧：把对端送来的 WireGuard 报文写进本机 wg，并把回程报文送回对端。
///
/// 用「对端索引 + portBase」绑定的本地端口收发：写进本机 wg 的源端口正好是
/// 本机给该 peer 配置的 endpoint，因此两侧 WireGuard 都不会因为源地址变化
/// 而把连接 roaming 到别处。
async fn wgtunnel_handler(
    State(state): State<AppState>,
    req: Request,
) -> Result<Response, (StatusCode, String)> {
    let caller = req
        .headers()
        .get("x-cluster-node")
        .and_then(|value| value.to_str().ok())
        .map(str::to_string)
        .ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                "缺少 x-cluster-node 头".to_string(),
            )
        })?;
    let Some(wg) = state.wg.clone() else {
        return Err((
            StatusCode::NOT_IMPLEMENTED,
            "本节点未启用 wgTunnel".to_string(),
        ));
    };
    let Some(channel) = wg.channel(&caller).cloned() else {
        return Err((
            StatusCode::BAD_REQUEST,
            format!("没有为 {caller} 绑定隧道端口"),
        ));
    };

    let target = wg.local_wg_addr();
    let socket = channel.socket.clone();
    let body = req.into_body();

    // 请求体：对端 -> 本机 wg
    let (done_tx, done_rx) = oneshot::channel::<()>();
    {
        let socket = socket.clone();
        let stats = channel.stats.clone();
        let sessions = state.wg_sessions.clone();
        let caller = caller.clone();
        sessions.lock().await.insert(caller.clone());
        // 接收方也要把对端登记成直连：对端在 NAT 后面时本机探测不到它。
        register_tunnel_peer(&state, &caller).await;
        recompute_routes(&state).await;
        let task_state = state.clone();
        tokio::spawn(async move {
            let mut incoming = body.into_data_stream();
            let mut framer = FrameReader::new();
            while let Some(chunk) = incoming.next().await {
                let Ok(chunk) = chunk else {
                    break;
                };
                framer.feed(chunk);
                while let Some(payload) = framer.next_frame() {
                    stats.from_peer.fetch_add(1, Ordering::Relaxed);
                    if socket.send_to(&payload, target).await.is_err() {
                        break;
                    }
                }
            }
            sessions.lock().await.remove(&caller);
            unregister_tunnel_peer(&task_state, &caller).await;
            recompute_routes(&task_state).await;
            let _ = done_tx.send(());
        });
    }

    // 响应体：本机 wg -> 对端
    let stats = channel.stats.clone();
    let frames = stream::unfold(
        (socket, done_rx, vec![0u8; 65535]),
        move |(socket, mut done, mut buf)| {
            let stats = stats.clone();
            async move {
                tokio::select! {
                    _ = &mut done => None,
                    received = socket.recv_from(&mut buf) => match received {
                        Ok((n, _)) => {
                            stats.to_peer.fetch_add(1, Ordering::Relaxed);
                            let frame = Bytes::from(encode_frame(&buf[..n]));
                            Some((Ok::<_, std::io::Error>(frame), (socket, done, buf)))
                        }
                        Err(_) => None,
                    },
                }
            }
        },
    );
    Ok(Response::new(Body::from_stream(frames)))
}

// ------------------------------------------------------------------ 刷新逻辑

#[derive(Debug)]
struct RefreshSummary {
    direct: usize,
    edges: usize,
    links: std::result::Result<(), String>,
}

impl Default for RefreshSummary {
    fn default() -> Self {
        Self {
            direct: 0,
            edges: 0,
            links: Ok(()),
        }
    }
}

/// 对“只能经 WireGuard 集群地址探测到”的节点只提示一次，避免每轮探测刷日志。
async fn note_inferred(state: &AppState, links: &[crate::topology::Link]) {
    let mut known = state.inferred_nodes.lock().await;
    let fresh: Vec<&str> = links
        .iter()
        .filter(|link| matches!(link.kind, crate::topology::LinkKind::Inferred))
        .filter(|link| known.insert(link.peer.clone()))
        .map(|link| link.peer.as_str())
        .collect();
    if !fresh.is_empty() {
        warn!(
            nodes = %fresh.join(","),
            "这些节点的 localIp/publicIp 都不可达，只能经 WireGuard 集群地址探测；请检查配置里的 localIp 是否已过期"
        );
    }
}

/// 立刻探测一遍、跟邻居同步一次拓扑、重算路由并落盘计划。
async fn refresh(state: &AppState) -> RefreshSummary {
    let mut summary = RefreshSummary::default();

    let links = probe::probe_all(
        &state.peers,
        &state.identity,
        &state.config,
        state.probe_timeout(),
    )
    .await;
    summary.direct = links.len();
    note_inferred(state, &links).await;
    let peers: Vec<(String, SocketAddr)> = links
        .iter()
        .map(|link| (link.peer.clone(), SocketAddr::new(link.address, link.port)))
        .collect();
    {
        let mut topo = state.topology.write().await;
        let changed = topo.set_local(links);
        topo.prune(gossip_ttl(&state.config));
        if changed {
            debug!(version = topo.version(), "直连结构变化，拓扑版本提升");
        }
    }
    *state.last_probe_at.write().await = Some(auth::now_unix());

    let mut errors = Vec::new();
    for (name, addr) in peers {
        match state
            .peers
            .get_json::<GraphResponse>(
                addr,
                &name,
                &state.identity,
                "/v1/graph",
                state.probe_timeout(),
            )
            .await
        {
            Ok(graph) => {
                let (accepted, rejected) = merge_graph(state, &name, graph).await;
                debug!(node = %name, accepted, rejected, "同步邻居拓扑");
            }
            Err(err) => {
                debug!(node = %name, error = %err, "拉取邻居拓扑失败");
                errors.push(format!("{name}: {err}"));
            }
        }
    }
    summary.links = if errors.is_empty() {
        Ok(())
    } else {
        Err(errors.join("; "))
    };
    // 「间接边」= 合并图里除本机直连之外的边。
    summary.edges = {
        let topo = state.topology.read().await;
        topo.graph_edges(&state.config)
            .len()
            .saturating_sub(topo.local.len())
    };

    let (table, topo_snapshot) = {
        let topo = state.topology.read().await;
        (route::compute(&state.config, &topo), topo.clone())
    };
    *state.routes.write().await = table.clone();
    let plan = wg_plan_for(state, &topo_snapshot, &table).await;
    publish_plan(state, plan).await;
    summary
}

/// 按当前拓扑与路由表生成 WireGuard 下发计划（含隧道端口映射）。
///
/// 需要读内核现状（allowed-ips）与隧道会话状态：allowed-ips 要增量合并，
/// endpoint 只有在隧道会话真正建立后才切到本地端口。
async fn wg_plan_for(state: &AppState, topo: &Topology, table: &RouteTable) -> route::WgPlan {
    let ports = match &state.wg {
        Some(wg) => wg.ports(),
        None => BTreeMap::new(),
    };
    let sessions = state.wg_sessions.lock().await.clone();
    let existing = route::current_allowed_ips(&state.config.service.wg_interface).await;
    let input = route::WgPlanInput {
        tunnel_ports: &ports,
        sessions: &sessions,
        existing_allowed_ips: &existing,
    };
    route::wg_plan(&state.config, topo, table, &input)
}

/// 把 WireGuard 计划落盘；wg/ip 命令没变化时不重复执行。
async fn publish_plan(state: &AppState, plan: route::WgPlan) {
    let run = {
        let mut last = state.last_commands.lock().await;
        if *last == plan.commands {
            false
        } else {
            *last = plan.commands.clone();
            true
        }
    };
    match route::apply(&plan, &state.config, run).await {
        Ok(report) => {
            if report.applied {
                let failed = report.commands.iter().filter(|c| !c.ok).count();
                if failed > 0 {
                    warn!(failed, "部分 wg/ip 命令执行失败");
                }
            }
            *state.last_apply.write().await = Some(report);
        }
        Err(err) => warn!(error = %err, "写入 wg 计划失败"),
    }
}

/// 隧道会话变化后立刻重算路由并下发计划：发起方要切 endpoint，接收方要补上
/// 「对端可达」这条事实，都等不到下一个路由周期。
async fn recompute_routes(state: &AppState) {
    let (table, topo_snapshot) = {
        let topo = state.topology.read().await;
        (route::compute(&state.config, &topo), topo.clone())
    };
    *state.routes.write().await = table.clone();
    let plan = wg_plan_for(state, &topo_snapshot, &table).await;
    publish_plan(state, plan).await;
}

/// 隧道会话建立：把对端登记成本机直连。对端可能在 NAT 后面，本机根本探测
/// 不到它的 peer 端口，但隧道已经跑通，这条链路是真实存在的。
async fn register_tunnel_peer(state: &AppState, peer: &str) {
    let Some(node) = state.config.node(peer) else {
        return;
    };
    let Some(ip) = node.cluster_ip else {
        return;
    };
    let port = node.peer_port(&state.config.service);
    let changed = {
        let mut topo = state.topology.write().await;
        topo.set_tunnel(peer, IpAddr::V4(ip), port)
    };
    if changed {
        info!(%peer, %ip, port, "把隧道对端登记为直连（本机探测不到它时靠这条链路）");
    }
}

/// 隧道会话断开：撤销隧道直连，避免继续把包送进已经不存在的会话。
async fn unregister_tunnel_peer(state: &AppState, peer: &str) {
    let changed = {
        let mut topo = state.topology.write().await;
        topo.clear_tunnel(peer)
    };
    if changed {
        warn!(%peer, "隧道会话断开，撤销隧道直连");
    }
}

// ---------------------------------------------------------------- 后台任务

async fn probe_loop(state: AppState) {
    let interval = Duration::from_millis(state.config.service.probe_interval_ms.max(500));
    loop {
        let links = probe::probe_all(
            &state.peers,
            &state.identity,
            &state.config,
            state.probe_timeout(),
        )
        .await;
        debug!(links = links.len(), "探测完成");
        note_inferred(&state, &links).await;
        {
            let mut topo = state.topology.write().await;
            topo.set_local(links);
            // 没刷新到拓扑也要按发布时刻过期，否则源下线后它的记录会一直留着。
            topo.prune(gossip_ttl(&state.config));
        }
        *state.last_probe_at.write().await = Some(auth::now_unix());
        tokio::time::sleep(interval).await;
    }
}

/// 监听内核网络事件：链路状态或 IP 地址一变就立刻重新探测。
///
/// 只订阅链路与地址事件（见 netwatch.rs），不订阅路由事件，避免本服务自己
/// 执行 ip route replace 时把自己反复触发。
async fn netwatch_loop(state: AppState) {
    if !state.config.service.net_watch.enable {
        info!("网络事件监听已关闭，网络变化只靠定时探测发现");
        return;
    }
    let quiet = Duration::from_millis(state.config.service.net_watch.debounce_ms.max(100));
    let mut watcher = match netwatch::Watcher::new() {
        Ok(watcher) => watcher,
        Err(err) => {
            warn!(error = %err, "监听内核网络事件失败，网络变化只能靠定时探测发现");
            return;
        }
    };
    info!(
        debounce_ms = quiet.as_millis() as u64,
        "已监听内核网络事件（链路与地址变化）"
    );
    loop {
        match watcher.wait_change(quiet).await {
            Ok(event) => {
                let change = event.describe();
                state.net_events.fetch_add(1, Ordering::Relaxed);
                info!(change = %change, "检测到网络变化，重新探测");
                *state.last_net_event.write().await = Some(NetEventInfo {
                    change,
                    at: auth::now_unix(),
                });
                let summary = refresh(&state).await;
                info!(
                    direct = summary.direct,
                    edges = summary.edges,
                    "网络变化触发的探测完成"
                );
            }
            Err(err) => {
                warn!(error = %err, "读取内核网络事件失败，5 秒后重新订阅");
                tokio::time::sleep(Duration::from_secs(5)).await;
                match netwatch::Watcher::new() {
                    Ok(fresh) => watcher = fresh,
                    Err(err) => {
                        warn!(error = %err, "重新订阅内核网络事件失败，改为定时探测");
                        return;
                    }
                }
            }
        }
    }
}

async fn gossip_loop(state: AppState) {
    let interval = Duration::from_millis(state.config.service.gossip_interval_ms.max(1000));
    loop {
        tokio::time::sleep(interval).await;
        let peers: Vec<(String, SocketAddr)> = {
            let topo = state.topology.read().await;
            // 隧道会话确认的对端也要拉：对端在 NAT 后时本机探测不到它，
            // 但隧道跑通后经集群地址能访问到它的 peer 端口。
            topo.local_links()
                .iter()
                .map(|(name, link)| (name.clone(), SocketAddr::new(link.address, link.port)))
                .collect()
        };
        for (name, addr) in peers {
            match state
                .peers
                .get_json::<GraphResponse>(
                    addr,
                    &name,
                    &state.identity,
                    "/v1/graph",
                    state.probe_timeout(),
                )
                .await
            {
                Ok(graph) => {
                    let (accepted, rejected) = merge_graph(&state, &name, graph).await;
                    debug!(node = %name, accepted, rejected, "同步邻居拓扑");
                }
                Err(err) => debug!(node = %name, error = %err, "同步邻居拓扑失败"),
            }
        }
    }
}

async fn route_loop(state: AppState) {
    let interval = Duration::from_millis(state.config.service.route_interval_ms.max(500));
    loop {
        tokio::time::sleep(interval).await;
        let (table, topo_snapshot) = {
            let topo = state.topology.read().await;
            (route::compute(&state.config, &topo), topo.clone())
        };
        *state.routes.write().await = table.clone();
        let plan = wg_plan_for(&state, &topo_snapshot, &table).await;
        publish_plan(&state, plan).await;
    }
}

// ------------------------------------------------------------------- 隧道

async fn peer_addr(state: &AppState, node: &str) -> Option<SocketAddr> {
    {
        let topo = state.topology.read().await;
        if let Some(link) = topo.direct(node) {
            return Some(SocketAddr::new(link.address, link.port));
        }
    }
    let cfg = state.config.node(node)?;
    let port = cfg.peer_port(&state.config.service);
    cfg.all_addresses()
        .first()
        .map(|ip| SocketAddr::new(IpAddr::V4(*ip), port))
}

async fn tunnel_session(
    state: AppState,
    via: String,
    target: SocketAddr,
    client_socket: Arc<UdpSocket>,
    client: SocketAddr,
    mut rx: mpsc::Receiver<Vec<u8>>,
) -> Result<()> {
    let addr = peer_addr(&state, &via)
        .await
        .with_context(|| format!("找不到中转节点 {via} 的地址"))?;

    let (body_tx, body_rx) = mpsc::channel::<Vec<u8>>(256);
    tokio::spawn(async move {
        while let Ok(Some(payload)) = tokio::time::timeout(Duration::from_secs(60), rx.recv()).await
        {
            if body_tx.send(payload).await.is_err() {
                break;
            }
        }
    });

    let frames = stream::unfold(body_rx, |mut rx| async move {
        rx.recv().await.map(|payload| (payload, rx))
    })
    .map(|payload| Ok::<_, std::io::Error>(Bytes::from(encode_frame(&payload))));
    let body = Body::from_stream(frames);
    let uri = format!("https://{via}/v1/tunnel?target={target}");

    let incoming = state
        .peers
        .post_stream(addr, &via, &state.identity, &uri, body)
        .await?;
    let mut incoming = Body::new(incoming).into_data_stream();
    let mut reader = FrameReader::new();
    while let Some(chunk) = incoming.next().await {
        let chunk = chunk.context("读取隧道响应失败")?;
        reader.feed(chunk);
        while let Some(payload) = reader.next_frame() {
            client_socket
                .send_to(&payload, client)
                .await
                .with_context(|| format!("回写 UDP 客户端 {client} 失败"))?;
        }
    }
    Ok(())
}

async fn spawn_tunnel(state: AppState, tunnel: Tunnel) {
    let bind = tunnel.listen;
    let socket = match UdpSocket::bind(bind).await {
        Ok(socket) => Arc::new(socket),
        Err(err) => {
            warn!(%bind, error = %err, "绑定隧道监听失败");
            return;
        }
    };
    info!(%bind, via = %tunnel.via, target = %tunnel.target, "启动 UDP 隧道");
    let sessions: Arc<Mutex<HashMap<SocketAddr, mpsc::Sender<Vec<u8>>>>> =
        Arc::new(Mutex::new(HashMap::new()));
    tokio::spawn(async move {
        let mut buf = vec![0u8; 65535];
        loop {
            let (n, from) = match socket.recv_from(&mut buf).await {
                Ok(pair) => pair,
                Err(err) => {
                    warn!(error = %err, "读取隧道 UDP 失败");
                    continue;
                }
            };
            let payload = buf[..n].to_vec();
            let mut guard = sessions.lock().await;
            let sender = match guard.get(&from).cloned() {
                Some(sender) => sender,
                None => {
                    let (tx, rx) = mpsc::channel(256);
                    guard.insert(from, tx.clone());
                    let state = state.clone();
                    let socket = socket.clone();
                    let via = tunnel.via.clone();
                    let target = tunnel.target;
                    tokio::spawn(async move {
                        if let Err(err) = tunnel_session(state, via, target, socket, from, rx).await
                        {
                            debug!(%from, error = %err, "隧道会话结束");
                        }
                    });
                    tx
                }
            };
            drop(guard);
            if sender.send(payload).await.is_err() {
                sessions.lock().await.remove(&from);
            }
        }
    });
}

/// 发起侧：把本机 WireGuard 发往 peer 的报文送进 h2 长连接，
/// 再把对端回来的报文写回本机 WireGuard 监听端口。
async fn wg_tunnel_client(state: AppState, channel: Arc<wgtunnel::Channel>) {
    let peer = channel.peer.clone();
    let Some(wg) = state.wg.clone() else {
        return;
    };
    let target = wg.local_wg_addr();
    let socket = channel.socket.clone();

    loop {
        // 只有需要代理的直连链路（默认 WAN，proxyLan 时含 LAN）才接管：
        // 经 WireGuard 中转的链路再走隧道会绕圈。
        let usable = {
            let topo = state.topology.read().await;
            let kind = topo.direct(&peer).map(|link| link.kind);
            // 只对探测到的 Lan/Wan 链路发起会话：Tunnel 链路本身就是会话的结果，
            // 拿它再发起会和对端互相套娃。
            kind.is_some_and(|kind| {
                matches!(
                    kind,
                    crate::topology::LinkKind::Lan | crate::topology::LinkKind::Wan
                ) && wgtunnel::should_proxy(&state.config.service.wg_tunnel, kind)
            })
        };
        let addr = if usable {
            peer_addr(&state, &peer).await
        } else {
            None
        };
        let Some(addr) = addr else {
            tokio::time::sleep(Duration::from_secs(2)).await;
            continue;
        };

        let (tx, rx) = mpsc::channel::<Vec<u8>>(256);
        let reader = {
            let socket = socket.clone();
            let stats = channel.stats.clone();
            tokio::spawn(async move {
                let mut buf = vec![0u8; 65535];
                loop {
                    match socket.recv(&mut buf).await {
                        Ok(n) => {
                            stats.to_peer.fetch_add(1, Ordering::Relaxed);
                            if tx.send(buf[..n].to_vec()).await.is_err() {
                                break;
                            }
                        }
                        Err(err) => {
                            debug!(error = %err, "读取 WireGuard 隧道端口失败");
                            break;
                        }
                    }
                }
            })
        };

        let frames = stream::unfold(rx, |mut rx| async move {
            rx.recv().await.map(|payload| (payload, rx))
        })
        .map(|payload| Ok::<_, std::io::Error>(Bytes::from(encode_frame(&payload))));
        let uri = format!("https://{peer}/v1/wgtunnel");
        let session = state
            .peers
            .post_stream(
                addr,
                &peer,
                &state.identity,
                &uri,
                Body::from_stream(frames),
            )
            .await;
        let incoming = match session {
            Ok(incoming) => incoming,
            Err(err) => {
                debug!(%peer, error = %err, "建立 WireGuard 隧道失败");
                reader.abort();
                tokio::time::sleep(Duration::from_secs(2)).await;
                continue;
            }
        };
        info!(%peer, %addr, "WireGuard 隧道已建立");
        state.wg_sessions.lock().await.insert(peer.clone());
        register_tunnel_peer(&state, &peer).await;
        recompute_routes(&state).await;

        let mut incoming = Body::new(incoming).into_data_stream();
        let mut framer = FrameReader::new();
        while let Some(chunk) = incoming.next().await {
            let Ok(chunk) = chunk else {
                break;
            };
            framer.feed(chunk);
            while let Some(payload) = framer.next_frame() {
                channel.stats.from_peer.fetch_add(1, Ordering::Relaxed);
                if let Err(err) = socket.send_to(&payload, target).await {
                    debug!(error = %err, "写回本机 WireGuard 失败");
                    break;
                }
            }
        }

        reader.abort();
        state.wg_sessions.lock().await.remove(&peer);
        unregister_tunnel_peer(&state, &peer).await;
        recompute_routes(&state).await;
        debug!(%peer, "WireGuard 隧道断开，2 秒后重连");
        tokio::time::sleep(Duration::from_secs(2)).await;
    }
}

// -------------------------------------------------------------- 监听与启动

pub fn peer_router(state: AppState) -> Router {
    Router::new()
        .route("/v1/ping", get(ping))
        .route("/v1/info", get(info))
        .route("/v1/graph", get(graph))
        .route("/v1/routes", get(routes_handler))
        .route("/v1/rescan", post(rescan))
        .route("/v1/tunnel", post(tunnel_handler))
        .route("/v1/wgtunnel", post(wgtunnel_handler))
        .route_layer(middleware::from_fn_with_state(state.clone(), auth_layer))
        .with_state(state)
}

pub fn local_router(state: AppState) -> Router {
    Router::new()
        .route("/v1/status", get(status))
        .route("/v1/graph", get(graph))
        .route("/v1/routes", get(routes_handler))
        .route("/v1/rescan", post(rescan))
        .route("/v1/ping", get(ping))
        .with_state(state)
}

async fn serve_peer(listener: TcpListener, acceptor: TlsAcceptor, router: Router) {
    loop {
        let (tcp, addr) = match listener.accept().await {
            Ok(pair) => pair,
            Err(err) => {
                warn!(error = %err, "接受 TCP 连接失败");
                tokio::time::sleep(Duration::from_millis(200)).await;
                continue;
            }
        };
        let acceptor = acceptor.clone();
        let router = router.clone();
        tokio::spawn(async move {
            let tls = match acceptor.accept(tcp).await {
                Ok(tls) => tls,
                Err(err) => {
                    debug!(%addr, error = %err, "TLS 握手失败");
                    return;
                }
            };
            let service = TowerToHyperService::new(router);
            let builder = hyper::server::conn::http2::Builder::new(TokioExecutor::new());
            if let Err(err) = builder.serve_connection(TokioIo::new(tls), service).await {
                debug!(%addr, error = %err, "HTTP/2 连接结束");
            }
        });
    }
}

async fn serve_unix(path: PathBuf, router: Router) -> Result<()> {
    if path.exists() {
        std::fs::remove_file(&path)
            .with_context(|| format!("删除旧 socket {} 失败", path.display()))?;
    }
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("创建 socket 目录 {} 失败", parent.display()))?;
    }
    let listener = UnixListener::bind(&path)
        .with_context(|| format!("绑定 unix socket {} 失败", path.display()))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o660));
    }
    info!(socket = %path.display(), "本地 CLI socket 已就绪");
    loop {
        let (stream, _) = listener.accept().await?;
        let router = router.clone();
        tokio::spawn(async move {
            let service = TowerToHyperService::new(router);
            if let Err(err) = hyper::server::conn::http1::Builder::new()
                .serve_connection(TokioIo::new(stream), service)
                .await
            {
                debug!(error = %err, "unix socket 连接结束");
            }
        });
    }
}

/// 服务入口：加载配置、启动监听与后台任务，直到收到 Ctrl-C。
pub async fn run(config_path: PathBuf) -> Result<()> {
    crate::tls::init_crypto();
    let config = Config::load(&config_path)?;
    let config = Arc::new(config);

    let identity = Arc::new(Identity::load_or_generate(
        &config.self_node,
        &config.service.ssh_host_key,
        &config.service.state_dir,
    )?);
    if identity.generated {
        warn!(
            key = %identity.source.display(),
            "使用自动生成的临时 SSH host key；请把公钥写进各节点的 hostPubKey"
        );
    }
    if let Some(me) = config.node(&config.self_node) {
        if !me.host_pub_key.trim().is_empty() {
            match crate::auth::public_key_fingerprint(&me.host_pub_key) {
                Ok(fp) if fp == identity.fingerprint() => {}
                Ok(fp) => warn!(
                    configured = %fp,
                    local = %identity.fingerprint(),
                    "配置中 self 节点的 hostPubKey 与本地 SSH host key 不一致"
                ),
                Err(err) => warn!(error = %err, "配置中 self 节点的 hostPubKey 无法解析"),
            }
        }
    }

    let tls_identity = crate::tls::load_or_generate(
        &config.service.state_dir,
        &config.self_node,
        config.service.tls_cert.as_deref(),
        config.service.tls_key.as_deref(),
    )?;
    let server_tls = crate::tls::server_config(&tls_identity)?;

    let state = AppState::new(config.clone(), identity.clone())?;
    info!(
        node = %config.self_node,
        host_key = %identity.fingerprint(),
        peer_port = config.service.peer_port,
        "cluster-node 启动"
    );

    let bind = SocketAddr::new(IpAddr::V4(config.service.bind), config.service.peer_port);
    let listener = TcpListener::bind(bind)
        .await
        .with_context(|| format!("监听 {bind} 失败"))?;
    let acceptor = TlsAcceptor::from(server_tls);
    tokio::spawn(serve_peer(listener, acceptor, peer_router(state.clone())));

    tokio::spawn(serve_unix(
        config.service.socket.clone(),
        local_router(state.clone()),
    ));

    tokio::spawn(probe_loop(state.clone()));
    tokio::spawn(netwatch_loop(state.clone()));
    tokio::spawn(gossip_loop(state.clone()));
    tokio::spawn(route_loop(state.clone()));

    if config.service.dns.enable {
        let dns_bind =
            SocketAddr::new(IpAddr::V4(config.service.dns.bind), config.service.dns.port);
        let dns_service = DnsService {
            config: config.clone(),
            routes: state.routes.clone(),
        };
        tokio::spawn(async move {
            if let Err(err) = dns::serve(dns_bind, dns_service).await {
                warn!(error = %err, "DNS 服务退出");
            }
        });
    }

    for tunnel in config.service.tunnels.clone() {
        spawn_tunnel(state.clone(), tunnel).await;
    }

    if let Some(wg) = state.wg.clone() {
        let mut initiated = 0;
        for channel in wg.channels() {
            // 只有名字较小的一侧发起，避免双向各建一条造成重复投递。
            if wgtunnel::initiates(&config, &channel.peer) {
                initiated += 1;
                tokio::spawn(wg_tunnel_client(state.clone(), channel.clone()));
            }
        }
        info!(
            channels = wg.ports().len(),
            initiated,
            wg_port = wg.wg_port,
            "WireGuard 自动隧道已启用"
        );
    }

    tokio::signal::ctrl_c().await.context("等待 Ctrl-C 失败")?;
    info!("收到退出信号，停止服务");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expired_records_are_never_merged() {
        let now = 1_000u64;
        let ttl = Duration::from_secs(60);
        let mut record = SourceRecord {
            source: "b".to_string(),
            version: 1,
            generated_at: 950,
            edges: Vec::new(),
        };
        assert!(!record_expired(&record, now, ttl));
        record.generated_at = 900;
        assert!(record_expired(&record, now, ttl));
    }
}
