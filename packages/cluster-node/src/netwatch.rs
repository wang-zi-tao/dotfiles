//! 监听内核 netlink 路由事件，在网络环境变化时立即触发重新探测（rescan）。
//!
//! 为什么用 netlink 而不是 D-Bus：
//! - NetworkManager / systemd-networkd / wpa_supplicant 各自只在自己运行的机器上提供信号。
//!   本集群里 nuc/asus/pc 跑 NetworkManager，而 aliyun 上的服务器既没有 NetworkManager
//!   也没有 systemd-networkd（dhcpcd + 内核地址），D-Bus 覆盖不全；
//! - 链路与地址变化的最终来源都是内核，NETLINK_ROUTE 组播在所有这些机器上都存在，
//!   而且不需要引入 D-Bus 客户端依赖；
//! - D-Bus 侧（NetworkManager）信号噪声更大，AccessPoint 属性每秒都在变，还要再翻译一层。
//!
//! 只订阅「链路状态」与「IP 地址增删」两组事件，不订阅路由变化：本服务的 wg 计划自己会
//! 执行 ip route replace，订阅路由会被自己的写操作反复触发。

use std::os::fd::{AsRawFd, OwnedFd};
use std::time::Duration;

use anyhow::{Context, Result};
use nix::net::if_::if_indextoname;
use nix::sys::socket::{
    self, AddressFamily, MsgFlags, NetlinkAddr, SockFlag, SockProtocol, SockType,
};
use tokio::io::unix::AsyncFd;
use tokio::time::Instant;

/// 组播组编号（见 linux/rtnetlink.h）。
const RTNLGRP_LINK: u32 = 1;
const RTNLGRP_IPV4_IFADDR: u32 = 5;
const RTNLGRP_IPV6_IFADDR: u32 = 6;

/// netlink 消息类型（见 linux/rtnetlink.h）。
const RTM_NEWLINK: u16 = 16;
const RTM_DELLINK: u16 = 17;
const RTM_NEWADDR: u16 = 20;
const RTM_DELADDR: u16 = 21;

/// IFF_UP 标志位。
const IFF_UP: u32 = 0x1;

/// nlmsghdr 的长度。
const NLMSG_HDR_LEN: usize = 16;

/// 把组号列表换算成 sockaddr_nl.nl_groups 用的掩码（组号从 1 开始）。
fn group_mask(groups: &[u32]) -> u32 {
    groups
        .iter()
        .fold(0u32, |mask, group| mask | (1u32 << (group - 1)))
}

/// 一条内核网络事件。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NetworkEvent {
    /// 链路状态变化。link_up 为 None 表示设备被删除。
    Link {
        ifname: Option<String>,
        /// 接口索引，设备删除后已经查不到名字，用它兜底。
        index: u32,
        link_up: Option<bool>,
    },
    /// IP 地址增删。family 是 AF_INET(2) 或 AF_INET6(10)。
    Address {
        ifname: Option<String>,
        index: u32,
        added: bool,
        family: u8,
    },
}

impl NetworkEvent {
    /// 是否值得触发重新探测：loopback 的抖动没有意义，其余都算。
    pub fn relevant(&self) -> bool {
        match self {
            Self::Link { ifname, .. } | Self::Address { ifname, .. } => {
                ifname.as_deref() != Some("lo")
            }
        }
    }

    /// 中文描述，用于日志与 CLI 状态。
    pub fn describe(&self) -> String {
        match self {
            Self::Link {
                ifname,
                index,
                link_up: Some(up),
            } => {
                let name = label(ifname, *index);
                if *up {
                    format!("接口 {name} 变为 UP")
                } else {
                    format!("接口 {name} 变为 DOWN")
                }
            }
            Self::Link {
                ifname,
                index,
                link_up: None,
            } => format!("接口 {} 被删除", label(ifname, *index)),
            Self::Address {
                ifname,
                index,
                added,
                family,
            } => {
                let name = label(ifname, *index);
                let family = match *family {
                    2 => "IPv4",
                    10 => "IPv6",
                    _ => "IP",
                };
                if *added {
                    format!("接口 {name} 新增 {family} 地址")
                } else {
                    format!("接口 {name} 删除 {family} 地址")
                }
            }
        }
    }
}

/// 接口的显示名：设备已删除时用索引兜底。
fn label(ifname: &Option<String>, index: u32) -> String {
    ifname.clone().unwrap_or_else(|| format!("#{index}"))
}

/// 解析一个 netlink 数据报里的所有事件。
fn parse_messages(buf: &[u8]) -> Vec<NetworkEvent> {
    let mut events = Vec::new();
    let mut offset = 0usize;
    while offset + NLMSG_HDR_LEN <= buf.len() {
        let Some(len) = read_u32(buf, offset).map(|len| len as usize) else {
            break;
        };
        let Some(kind) = read_u16(buf, offset + 4) else {
            break;
        };
        if len < NLMSG_HDR_LEN || offset + len > buf.len() {
            break;
        }
        let payload = &buf[offset + NLMSG_HDR_LEN..offset + len];
        let event = match kind {
            // ifinfomsg: family u8, pad u8, type u16, index i32, flags u32, change u32
            RTM_NEWLINK | RTM_DELLINK if payload.len() >= 16 => {
                let index = read_u32(payload, 4).unwrap_or(0);
                let flags = read_u32(payload, 8).unwrap_or(0);
                Some(NetworkEvent::Link {
                    ifname: ifname(index),
                    index,
                    link_up: (kind == RTM_NEWLINK).then_some(flags & IFF_UP != 0),
                })
            }
            // ifaddrmsg: family u8, prefixlen u8, flags u8, scope u8, index u32
            RTM_NEWADDR | RTM_DELADDR if payload.len() >= 8 => {
                let index = read_u32(payload, 4).unwrap_or(0);
                Some(NetworkEvent::Address {
                    ifname: ifname(index),
                    index,
                    added: kind == RTM_NEWADDR,
                    family: payload[0],
                })
            }
            _ => None,
        };
        events.extend(event);
        // 每条消息按 4 字节对齐。
        offset += (len + 3) & !3;
    }
    events
}

/// 从缓冲区读一个小端/本机端序的无符号整数（越界返回 None）。
fn read_u16(buf: &[u8], at: usize) -> Option<u16> {
    let end = at.checked_add(2)?;
    buf.get(at..end)?.try_into().ok().map(u16::from_ne_bytes)
}

fn read_u32(buf: &[u8], at: usize) -> Option<u32> {
    let end = at.checked_add(4)?;
    buf.get(at..end)?.try_into().ok().map(u32::from_ne_bytes)
}

/// 接口索引转名字，设备已删除时返回 None。
fn ifname(index: u32) -> Option<String> {
    // nix 的 if_indextoname 在设备已删除时返回空字符串而不是错误（libc 返回 NULL
    // 被 Errno::result 当成成功），这里过滤掉空名字，改由调用方用索引兜底。
    if_indextoname(index)
        .ok()
        .and_then(|name| name.into_string().ok())
        .filter(|name| !name.is_empty())
}

/// netlink 事件监听器。
pub struct Watcher {
    fd: AsyncFd<OwnedFd>,
}

impl Watcher {
    /// 打开一个 NETLINK_ROUTE 组播 socket 并订阅链路与地址事件。
    ///
    /// 不需要 root 权限，普通用户也能收到这些组播事件。
    pub fn new() -> Result<Self> {
        let groups = group_mask(&[RTNLGRP_LINK, RTNLGRP_IPV4_IFADDR, RTNLGRP_IPV6_IFADDR]);
        let fd = socket::socket(
            AddressFamily::Netlink,
            SockType::Raw,
            SockFlag::SOCK_NONBLOCK | SockFlag::SOCK_CLOEXEC,
            SockProtocol::NetlinkRoute,
        )
        .context("创建 NETLINK_ROUTE socket 失败")?;
        socket::bind(fd.as_raw_fd(), &NetlinkAddr::new(0, groups))
            .context("订阅 NETLINK_ROUTE 组播失败")?;
        // SAFETY: OwnedFd 独占所有权并在 Drop 时关闭，AsyncFd 只是借用它的 fd 号。
        let fd = unsafe { AsyncFd::register(fd) }
            .map_err(|err| anyhow::anyhow!("注册 netlink socket 到 tokio 失败: {err}"))?;
        Ok(Self { fd })
    }

    /// 读一个数据报，返回其中第一条相关事件。
    ///
    /// 同一个数据报里的其他事件会被丢弃：它们属于同一波变化，
    /// 重新探测一次即可覆盖。
    pub async fn next_event(&mut self) -> Result<NetworkEvent> {
        loop {
            let mut guard = self.fd.readable().await.context("等待 netlink 事件失败")?;
            match guard.try_io(|inner: &AsyncFd<OwnedFd>| read_datagram(inner.get_ref())) {
                Ok(Ok(events)) => {
                    if let Some(event) = events.into_iter().find(NetworkEvent::relevant) {
                        return Ok(event);
                    }
                }
                Ok(Err(err)) => return Err(err).context("读取 netlink 事件失败"),
                // 还没有数据，继续等下一次可读。
                Err(_would_block) => continue,
            }
        }
    }

    /// 等一次网络变化，并把同一波变化合并成一次。
    ///
    /// 收到第一条事件后静默 quiet 时长（期间再来事件就重新计时），
    /// 总等待不超过 quiet * 5，避免持续抖动时一直不返回。
    pub async fn wait_change(&mut self, quiet: Duration) -> Result<NetworkEvent> {
        let start = Instant::now();
        let cap = quiet.saturating_mul(5);
        let mut last = self.next_event().await?;
        loop {
            let remaining = cap.saturating_sub(start.elapsed());
            if remaining.is_zero() {
                break;
            }
            match tokio::time::timeout(quiet.min(remaining), self.next_event()).await {
                Ok(Ok(event)) => last = event,
                Ok(Err(err)) => return Err(err),
                Err(_elapsed) => break,
            }
        }
        Ok(last)
    }
}

/// 从 socket 读一个数据报并解析。
fn read_datagram(fd: &OwnedFd) -> std::io::Result<Vec<NetworkEvent>> {
    let mut buf = [0u8; 8192];
    let len =
        socket::recv(fd.as_raw_fd(), &mut buf, MsgFlags::empty()).map_err(std::io::Error::from)?;
    Ok(parse_messages(&buf[..len]))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// 拼一条 netlink 消息（含 4 字节对齐填充）。
    fn message(kind: u16, payload: &[u8]) -> Vec<u8> {
        let len = NLMSG_HDR_LEN + payload.len();
        let padded = (len + 3) & !3;
        let mut buf = vec![0u8; padded];
        buf[0..4].copy_from_slice(&(len as u32).to_ne_bytes());
        buf[4..6].copy_from_slice(&kind.to_ne_bytes());
        buf[NLMSG_HDR_LEN..len].copy_from_slice(payload);
        buf
    }

    fn ifaddr(family: u8, index: u32) -> Vec<u8> {
        let mut payload = vec![0u8; 8];
        payload[0] = family;
        payload[1] = 24;
        payload[4..8].copy_from_slice(&index.to_ne_bytes());
        payload
    }

    fn ifinfo(index: i32, flags: u32) -> Vec<u8> {
        let mut payload = vec![0u8; 16];
        payload[4..8].copy_from_slice(&index.to_ne_bytes());
        payload[8..12].copy_from_slice(&flags.to_ne_bytes());
        payload
    }

    #[test]
    fn group_mask_sets_one_bit_per_group() {
        assert_eq!(group_mask(&[RTNLGRP_LINK]), 0b1);
        assert_eq!(
            group_mask(&[RTNLGRP_LINK, RTNLGRP_IPV4_IFADDR, RTNLGRP_IPV6_IFADDR]),
            0b110001
        );
    }

    #[test]
    fn parses_address_and_link_events_from_one_datagram() {
        let mut buf = message(RTM_NEWLINK, &ifinfo(1, IFF_UP));
        buf.extend(message(RTM_NEWADDR, &ifaddr(2, 1)));
        let events = parse_messages(&buf);
        assert_eq!(events.len(), 2);
        assert_eq!(
            events[0],
            NetworkEvent::Link {
                ifname: Some("lo".to_string()),
                index: 1,
                link_up: Some(true),
            }
        );
        assert_eq!(
            events[1],
            NetworkEvent::Address {
                ifname: Some("lo".to_string()),
                index: 1,
                added: true,
                family: 2,
            }
        );
    }

    #[test]
    fn loopback_and_unknown_messages_are_not_relevant() {
        let events = parse_messages(&message(RTM_DELADDR, &ifaddr(2, 1)));
        assert_eq!(events.len(), 1);
        assert!(!events[0].relevant());
        assert!(parse_messages(&message(2, &[0u8; 4])).is_empty());
    }

    #[test]
    fn truncated_datagram_is_ignored() {
        let mut buf = message(RTM_NEWADDR, &ifaddr(2, 3));
        buf.truncate(NLMSG_HDR_LEN + 2);
        assert!(parse_messages(&buf).is_empty());
    }

    #[test]
    fn describes_events_in_chinese() {
        let up = NetworkEvent::Link {
            ifname: Some("wlp132s0".to_string()),
            index: 5,
            link_up: Some(true),
        };
        assert_eq!(up.describe(), "接口 wlp132s0 变为 UP");
        let addr = NetworkEvent::Address {
            ifname: Some("wg0".to_string()),
            index: 8,
            added: false,
            family: 2,
        };
        assert_eq!(addr.describe(), "接口 wg0 删除 IPv4 地址");
        // 设备已删除时 if_indextoname 查不到名字，用索引兜底。
        let gone = NetworkEvent::Link {
            ifname: None,
            index: 7,
            link_up: None,
        };
        assert_eq!(gone.describe(), "接口 #7 被删除");
    }
}
