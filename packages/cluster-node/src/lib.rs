//! cluster-node：集群节点互联、探测、路由与隧道。
//!
//! 模块划分：
//! - [\`config\`]：json5 配置与校验
//! - [\`auth\`]：基于 ssh host key 的请求签名
//! - [\`tls\`]：自签证书与 TLS 配置
//! - [\`peer\`]：带认证的 HTTP/2 对等客户端
//! - [\`probe\`]：本机网卡探测与延迟测量
//! - [\`topology\`]：网络图与 gossip 合并
//! - [\`route\`]：最优路径计算与 wg 下发计划
//! - [\`dns\`]：按可达性解析 hostnames
//! - [\`tunnel\`]：UDP over HTTP/2 代理
//! - netwatch：内核 netlink 网络事件（链路 / 地址变化），触发重新探测
//! - wgtunnel：WireGuard 流量的自动代理（替代 udp2raw）
//! - [\`service\`]：axum 服务与后台任务
//! - [\`cli\`]：与服务通信的命令行客户端

pub mod auth;
pub mod cli;
pub mod config;
pub mod dns;
pub mod framing;
pub mod netwatch;
pub mod peer;
pub mod probe;
pub mod route;
pub mod service;
pub mod tls;
pub mod topology;
pub mod tunnel;
pub mod wgtunnel;
