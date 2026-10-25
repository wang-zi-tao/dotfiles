用于连接多个计算机， 自动探测本地网络联通性， 自动调节wireguard连接和路由

# features
- 自动同步网络图拓扑
- 发送心跳包
- 自动计算路由
- http2 自动代理 wireguard 流量（替代 udp2raw；跨网段默认代理，同网段可选）
- http2长连接， 自签证书
- 自动探测本地网络联通, 和延迟
- ssh host key签名验证
- 本地dns服务， 按照优先级查询集群内可联通主机
- 分为服务和命令行。 cli与服务之间通过socket+http通信
- cli 支持查询网络图, 和强制重新检测网络环境
# 架构
- rust语言
- p2p架构
- axum框架
- 图算法：petgraph（拓扑图上的加权最短路）
# 配置
nixos集群配置自动生成配置文件
```json5
{
    "self": "wangzi-nuc",
    "nodes":{
        "wangzi-nuc": {
            "localIp": ["192.168.32.1", "192.168.1.200"],
            "hostPubKey": ""
        },
        "wangzi-asus": {
            "localIp": ["192.168.32.1", "192.168.1.200"],
            "hostPubKey": ""
        },
        "aliyun-hk": {
            "publicIp": ["47.83.14.140"],
            "speed": 5, // MB, 用于计算路由图
            "hostPubKey": ""
        }
    },
    "hostnames":{
        "ollama.c.wg": {"hosts": ["wangzi-asus" "wangzi-nuc"] },
        "libvirt.c.wg": {"hosts": ["wangzi-asus" "wangzi-nuc"] },
    }
}
```

# 实现

## 代码结构
| 文件 | 职责 |
| --- | --- |
| `src/config.rs` | json5 配置解析（地址可写单个字符串或数组）、校验（悬空 hostname、重复 clusterIp、host key 有效性） |
| `src/auth.rs` | 加载 SSH host key（缺失时自动生成临时 key）、ed25519 签名/校验、时间窗、nonce 重放缓存 |
| `src/tls.rs` | rcgen 自签证书 + rustls 配置；对等连接 ALPN=h2 |
| `src/peer.rs` | HTTP/2 客户端：连接池、keep-alive、带签名请求、流式请求 |
| `src/probe.rs` | TCP 连通性/延迟探测、本机地址枚举、隧道目标白名单判断 |
| `src/topology.rs` | 拓扑同步：每个节点只发布自己实测的直连（信息源 + 版本号），按源合并、按发布时刻过期 |
| `src/route.rs` | petgraph 无向图上的加权最短路（边权 = rtt + 1000/speed ms）、路由表、WireGuard 计划生成与下发 |
| `src/dns.rs` | 最小 DNS 服务：按 hostnames.hosts 顺序返回第一个可联通节点 |
| `src/tunnel.rs` | UDP over HTTP/2 双向流（一个 h2 流承载一个 UDP 会话） |
| `src/wgtunnel.rs` | WireGuard 自动隧道：每个 peer 的 127.0.0.1 通道端口、代理决策、发起方判定 |
| `src/framing.rs` | 隧道帧格式（2 字节大端长度前缀） |
| `src/netwatch.rs` | 内核 netlink 事件监听（链路 / 地址变化），防抖后触发重新探测 |
| `src/service.rs` | axum 路由、后台循环（探测/gossip/路由）、unix socket、启动与关闭 |
| `src/cli.rs`, `src/bin/*` | 命令行：访问本地 socket，中文表格输出 |

## 对等 API（全部需要签名，走 HTTP/2）
- `GET /v1/ping`
- `GET /v1/info`：节点名、host key 指纹、peerPort、版本
- `GET /v1/graph`：本节点自己的直连 + 已收到的所有信息源记录（含 source/version/generatedAt，转述时原样保留来源）
- `GET /v1/routes`：当前路由表
- `POST /v1/rescan`：立即重新探测
- `POST /v1/tunnel`：建立 UDP 代理（`?target=ip:port`）
- `POST /v1/wgtunnel`：WireGuard 流量的自动代理通道（响应体是目标 UDP 的回复流）

## 身份认证
- 请求头：`x-cluster-node` / `x-cluster-time` / `x-cluster-nonce` / `x-cluster-signature`
- 待签内容：`cluster-node-auth-v1\n{METHOD}\n{PATH}\n{NODE}\n{TIME}\n{NONCE}`
- 签名：SSH host key（ed25519）；对端公钥来自配置里的 `hostPubKey`
- 时间窗默认 120s（`authSkewSecs`），nonce 在窗口内去重，防重放
- TLS 自签证书只负责加密；身份完全由上述签名保证（不依赖 WebPKI 与证书校验）

## 命令行
```bash
cluster-node --config /etc/cluster-node/config.json5            # 启动服务
cluster-node --config /etc/cluster-node/config.json5 --check    # 只校验配置
cluster-node --print-host-key                                   # 打印本机 host key 公钥与指纹
cluster-node-cli status        # 节点、直连邻居、路由表
cluster-node-cli graph         # 当前拓扑边
cluster-node-cli routes        # 路由表（--json 输出机器可读结果）
cluster-node-cli rescan        # 强制重新探测
```
命令行默认连接 `/run/cluster-node/node.sock`，可用 `--socket` 覆盖。

## NixOS 集成
- `module/cluster-node.nix` 从 `cluster.nodes` / `cluster.network.nodes` / `cluster.wireguard.nodes` 生成 `/etc/cluster-node/config.json5`，并启动 `cluster-node` systemd 服务（集群内默认开启）。
- 选项：`services.cluster-node.{peerPort,socket,stateDir,sshHostKey,cliGroup,applyKernel,wgInterface,dns.*,wgTunnel.*,netWatch.*,tunnels,extraSettings}`。
- `dns.port` 默认 5335（5353 常被 avahi/mDNS 占用，绑定会失败）；`cliGroup` 默认 `wheel`，决定谁能直接访问 CLI socket（socket 0660 + `/run/cluster-node` 0750）。
- `applyKernel = true`（默认）时执行 `wg set` / `ip route`（systemd 单元已加 `CAP_NET_ADMIN`，PATH 含 `wg`、`ip`）；设为 false 只把计划写到 `/var/lib/cluster-node/wg-plan.json`。
- `wgTunnel.{enable,portBase,proxyLan}` 控制自动代理（默认开启）；`module/wireguard.nix` 里 udp2raw 的 client/server 单元与 `channelPort` 端口已整体删除，
  endpoint 仍由 Nix 生成初值，运行时由 cluster-node 改写。
- `tunnels` 是手工兜底（固定 listen/via/target），自动代理覆盖不到的特殊流量再用它。

## WireGuard 自动隧道（替代 udp2raw）

跨网段的 WireGuard 流量默认不再依赖 udp2raw，而是由 cluster-node 在多条 HTTP/2 长连接上自动代理：

- **端口布局**：每个 peer 一个本地 UDP 端口 `127.0.0.1:(peer.index + portBase)`，`portBase` 默认 41000（沿用 udp2raw 时代的 `channelPort`）。
- **什么时候代理**：路由图算完后，`Wan`（跨网段）邻居的 endpoint 被改写成 `127.0.0.1:<本地端口>`；
  `Lan`（同网段）默认直连，`proxyLan = true` 才一起代理；`Inferred`（只能经 wg 集群地址发现，如云上的家宽节点）不代理，避免 wg-over-wg 绕圈。
   另外 endpoint 只在「与该对端的隧道会话已经建立」之后才切到本地端口，发起方和响应方都要切：
  响应方的 WireGuard 报文也要写进自己的隧道端口，由响应流送回对端。对端还是旧版本、会话建不起来时
  保持真实地址，不会把本来能直连的链路打断。
- **谁发起**：公网可达性优先——「本机无公网地址、对端有」时由本机发起（否则对端连不进 NAT 后面），
  「本机有公网地址、对端没有」时等对端连进来；两侧条件相同（都公网或都在 NAT 后）时由名字较小的一侧发起。
  同一条链路只会有一条会话——两侧都建会争抢同一个通道 socket 的读端，数据面会串包。
- **数据面**：发起方对 `POST /v1/wgtunnel` 发带签名的 HTTP/2 请求，请求体是从客户端 UDP 收到的包（2 字节大端长度前缀分帧），
  响应体是目标 UDP 的回复流。两侧各自看到的是「对端通道端口 -> 本机 wg 端口」，所以 WireGuard 认为对端就在本机。
- **生命周期**：一个 h2 流承载一个 UDP 会话，60 秒无流量即关闭；链路断开后 2 秒重连。`cluster-node-cli status` 会列出每个通道的收发计数与连接状态。
- **隧道会话确认的直连**：一个活着的隧道会话就是一条真实直连。公网节点探测不到 NAT 后面的对端
  （`localIp` 是内网地址、又没有 `publicIp`），可隧道已经跑通了；所以会话建立时把对端登记进拓扑
  （链路类型 `tunnel`，优先级排在 `wan` 之后、`inferred` 之前），会话断开时撤销，并立刻重算路由、下发计划。
  不这么做的话，响应方会把「探测不到的对端」判成不可达，反而把它的 clusterIp 从 allowed-ips 里清掉，
  表现为隧道计数器在涨、`ping` 却报 `sendmsg: Required key not available`（ENOKEY）。
- **allowed-ips 的归属**：WireGuard 的 allowed-ips 是全局唯一的「目的地址 -> peer」映射，整体替换时会把别的 peer 上的同名条目挤掉。
  因此这里**只把每个直连 peer 自己的 clusterIp 分给它**，内核里原有的其他网段（`10.16.x.0/24` 这类 Nix 配置的网段）原样保留，
  本机不再做 WireGuard 层面的多跳中转（多跳只会展示在 CLI 路由表里做诊断）。邻居的网段用配置里的 `clusterIpRange` 兜底恢复，
  这样即使内核里被旧版本清空过，下一次刷新也会补回来。
- **下发内核**：endpoint 改写写成 `wg set ... endpoint` 命令，`applyKernel` 为 true 时由 `sh -c` 执行（需要 `CAP_NET_ADMIN` 与 PATH 里的 `wg`）；否则只落到 `wg-plan.json`。

## 网络变化触发重扫（netwatch）

除了定时探测，cluster-node 还直接监听内核的 netlink 事件，网络一变化就立即重扫：

- **事件来源**：`NETLINK_ROUTE` 组播，订阅 `RTNLGRP_LINK`（链路 UP/DOWN、设备增删）、
  `RTNLGRP_IPV4_IFADDR` / `RTNLGRP_IPV6_IFADDR`（地址增删）。普通用户即可订阅，不需要 root。
- **为什么不走 D-Bus**：本集群里只有桌面/笔记本跑 NetworkManager，云服务器（如 aliyun-*）既没有
  NetworkManager 也没有 systemd-networkd，D-Bus 上根本没有可订阅的管理器；netlink 是所有 Linux 机器
  共同的一手事件源。另外 NetworkManager 的 `AccessPoint.PropertiesChanged` 在 wifi 扫描时会刷屏
  （实测一次 `nmcli device wifi rescan` 就是几十条），而且它并不直接表达「本机地址变了」。
- **不订阅路由组**：`IPV4_ROUTE` / `IPV6_ROUTE` 事件会被本服务自己下发的 `ip route replace` 触发，
  订阅它就等于自己触发自己（重扫 -> 下发路由 -> 重扫）。链路和地址变化才是「需要重新探测」的信号。
- **防抖**：一次 wifi 重连 / DHCP 续租通常连发 link + addr 好几条事件，`service.netWatch.debounceMs`
  （默认 1500ms）把静默窗口内的事件合并成一次重扫；上限是窗口的 5 倍，不会因为持续抖动一直不重扫。
- **过滤**：忽略 loopback；设备已删除、`if_indextoname` 查不到名字时用接口索引显示（如「接口 #10 被删除」）。
- **关闭**：`service.netWatch.enable = false`（或 `services.cluster-node.netWatch.enable = false`）后
  只保留 `probeIntervalMs` 的定时探测。
- **观测**：`cluster-node-cli status` 多一行「网络事件: 已因网络变化重新探测 N 次，最近: ...」，
  日志里则是「检测到网络变化，重新探测」+「网络变化触发的探测完成 direct=… edges=…」。

## 拓扑同步：信息源 + 版本号（向量时钟）

节点之间同步的不是「一张图」，而是「每个节点对自己直连的那一段事实」，等价于一个按信息源分桶的
向量时钟式分布式数据库：

- **只说自己**：一条记录 = {source, version, generatedAt, edges}，edges 里每条边都必须有一端是
  source。谁也不能替别人背书，伪造/越权的边在接收时直接丢掉。
- **版本只在结构变化时 +1**：对端集合、链路类型（Lan/Wan/Inferred）、对端地址任一变化才算「结构变化」，
  version 加一；只有 RTT 抖动时不动 version，只刷新 generatedAt（当作心跳）。
- **合并是 LWW**：按 source 分桶，比较 (version, generatedAt) 字典序，大者胜；完全相同则只更新接收时间
  （表示「源还活着」）。比本地更旧的版本（Stale）直接忽略。
- **生命周期看发布时刻**：TTL 默认 max(gossipIntervalMs × 6, 60s)，用 now - generatedAt 判断，不看接收时间。
  转述方每轮都会把同一条记录再发一遍，若按接收时间续命，源下线后它的旧信息会被永久转述下去
  （实测过：中继节点会把已下线节点的记录一遍遍「复活」）。
- **转述（中继）**：/v1/graph 返回本机记录 + 本机收到的所有源记录。接收方按 source 识别，所以 A 连不上 B、
  但 A、B 都能连上 C 时，A 依然能从 C 那里拿到 B 的直连信息（反过来也一样）。
- **自己的边只信自己**：别人记录里提到本机的边会照收、照转述（那是对方的说法），但合并成图时跳过。
  自己是否与某节点直连只能由本机探测决定，否则断链后邻居会一直替我们转述那条死边。
- **观测**：cluster-node-cli status 有「信息源」一节（source / vN / 多大年龄 / 几条边 / 是否本机），
  cluster-node-cli graph 显示本机版本、各信息源与合并后的边；日志里「丢弃过期的拓扑记录」
  「直连结构变化，拓扑版本提升」是 debug 级。

## 本地验证
```bash
cd packages/cluster-node
cargo test                                   # 54 个单元测试
# 起两个实例（各自一份 ssh host key 与配置）后：
cluster-node-cli --socket /tmp/a.sock status
cluster-node-cli --socket /tmp/a.sock rescan
```
已实测：两实例互相探测到 LAN 链路与 RTT、gossip 同步拓扑、自动路由（含 next hop/路径/代价）、
DNS 按 hosts 顺序返回可联通节点（A 记录 / NXDOMAIN / AAAA 空应答）、UDP over HTTP/2 隧道回环收发，
以及「localIp 全部不可达时经 clusterIp 兜底发现对端」（链路类型记为 inferred）。
netwatch 也已实测：创建带 IPv4 的 dummy 接口（link add + up + addr add 一波）只在防抖后触发 1 次重扫，
删除接口再触发 1 次（显示为「接口 #N 被删除」），静置期间不会自触发。
拓扑同步（信息源 + 版本号）也已实测：三节点 a/b/c，故意让 a 连不上 b（只让 a↔c、b↔c 通），
a 从 c 转述的记录里学到 c--b 边并算出「b via c」的路由；把 b 杀掉 60 秒后，a 与 c 上的 b 记录按发布时刻
过期，路由变成「不可达」，也不会被 c 的持续转述复活。
隧道会话确认的直连也已实测：让 b 的配置里 a 的地址不可达（b 探测不到 a），a 侧探测正常并建起隧道后，
b 的 status 直连里出现 `a 192.168.16.12 rtt=0.00ms "tunnel"`、路由表 `a via=a`，`wg-plan.json` 里
`allowedIps.a = [192.168.16.12]`、`endpoints.a = 127.0.0.1:44512`（b 自己的通道端口）。

## 路由与中转

- 最短路的**第一跳必须是本机实测过的直连邻居**（Lan/Wan，或隧道会话确认的 `tunnel`）。邻居转述的边只用于计算后面的跳数；
  别人记录里提到本机的边不参与合并——链路断开后邻居会继续转述断链前的那条直连边，最短路就会
  挑中这条死边，把包交给一个不可达的 endpoint（新版本按信息源管理，见上一节）。
- `wg_plan` 按「第一跳」分配 allowed-ips：非直连目的地的 clusterIp 分给中转 peer，同时从其它
  peer 上清掉残留的同一地址。allowed-ips 既是目的路由表、又是入站源地址白名单，两边都必须有，
  否则回程会按源地址校验丢弃（表现为握手正常、ping 不通）。
- 本机自己的 `Inferred` 边（只能经集群地址探测到）不当作直连；邻居的 `Inferred` 边可以当中转
  路径的一段。
- 中转能否成立还取决于中转节点自己能不能到达目标：目标在 NAT 后面又没有配置可达地址时，
  中转节点也过不去（这和 udp2raw 时代的限制一样，需要给目标配 publicIp 或端口转发）。

## 故障排查

- **DNS 绑定失败**：日志里出现「绑定 DNS 127.0.0.1:5353 失败」时，多半是 avahi/mDNS（或 systemd-resolved）已占用 5353。
  默认端口已改为 5335；如仍冲突，改 `services.cluster-node.dns.port`。
- **隧道已连接但 ping 不通 / `sendmsg: Required key not available`**：多半是响应方探测不到发起方
  （对端在 NAT 后、配置里没有 `publicIp`），把对端判成不可达后清掉了它的 clusterIp。新版本会把隧道会话
  确认成直连（见上一节），升级到该版本即可；顺手检查 `wg show wg0 allowed-ips` 里每个直连 peer 是否都
  拿到了自己的 `/32`。
- **路由变成单向 / 只发现部分节点**：先检查配置里的 `localIp` 是否已过期（DHCP 换地址很常见，
  真实网段地址与 `module/cluster.nix` 里写的对不上就会单向）。服务端按
  `localIp -> publicIp -> clusterIp`（经 WireGuard）顺序探测，最后一项是兜底：只要 WireGuard 通就能发现对端，
  链路类型记为 `inferred`，并在日志里对每个节点只提示一次，提示后请修正配置。
- **`status` 里网络事件次数不断增长**：先看是不是别的进程在反复改地址/链路（VPN、容器、NetworkManager
  重连都会产生事件）。本服务只订阅链路与地址组，自己下发的 `ip route` / `wg set` 不会触发重扫；
  觉得太灵敏可以调大 `service.netWatch.debounceMs`，或直接 `netWatch.enable = false` 退回定时探测。
- **非 root 跑 `cluster-node-cli` 报 Permission denied**：socket 属主是 `root`、组是 `cliGroup`（默认 `wheel`）。
  把用户加入该组即可，不需要 sudo。
- **日志出现「绑定 WireGuard 隧道端口失败 ... Address already in use」**：旧进程还占着 `index + portBase` 端口。
  正式环境用 `systemctl restart cluster-node`；本地同时跑两份配置做实验时，`portBase` 必须错开（或按 PID `kill -9` 清掉残留实例）。
- **隧道显示「已连接」但数据不通**：先看两侧 `status` 的收发计数。若一侧「本机->对端」在涨而对侧「对端->本机」不涨，
  说明对端在往错误地址回包（通常是残留旧进程或旧配置）。跨 NAT 的链路必须由 NAT 一侧发起，判定规则见上文。
- **集群里某个节点 ping 不通、但握手正常**：典型是 `wg show wg0 allowed-ips` 出现「同一个 clusterIp 挂在别人的 peer 上」或某个 peer 变成 `(none)`。
  旧版本把所有可达目的地都分给 `next hop`，中转会把直连 peer 的 `/32` 抢走；接收方按自己 peer 的 allowed-ips 校验源地址，源地址不匹配的包直接被丢。
  新版本每个直连 peer 只承载自己的 clusterIp，升级后用 `wg show wg0 allowed-ips` 确认即可；旧二进制会在每次刷新时把表改回去，必须所有运行 cluster-node 的节点一起升级。
- **`wg set` 报 Operation not permitted**：`applyKernel = true` 需要 `CAP_NET_ADMIN`；服务单元已声明该 capability，
  若自行改过 unit 或手动前台运行（非 root），请补上权限或先用 `applyKernel = false` 只生成计划。
- **`nix build .#cluster-node` 在远端 builder 上失败（cargo-vendor-dir, exit 1）**：多为 builder 磁盘满，
  可用 `nix build .#cluster-node --builders ''` 在本机构建。
- **直连断开后 `graph` 里还显示一条直连边、ping 却不通**：若是老版本，邻居转述的旧边被当成直连；
  新版本只信自己的探测，这条边不会进入合并图（在「信息源」里还能看到对方的说法，那是正常的转述）。
  若新版本里它仍显示为可路由，检查 `routes` 里该节点的 `nextHop` 是否为某个中转节点，
  以及中转节点自己能否到达它。
- **节点已经下线，路由表里却一直可达**：说明它的记录还在被转述。记录按 generatedAt 过期（默认 60 秒，
  或 gossipIntervalMs × 6），与接收时间无关。若超过 TTL 仍在，看是不是混进了老版本节点（它不会过期，
  会一直把旧记录发出来），全部升级后即可。
- **有线断了、只剩 wifi 时找不到对方**：`localIp` 只配了有线地址。把它写成数组（Nix 里用
  `cluster.network.nodes.<name>.config.ips` 追加额外地址），探测会逐个候选地址尝试。
- **中转节点也转发不出去**：确认中转节点自己有到目标的可用地址（`publicIp`/端口转发），
  并用 `cluster-node-cli routes` 看目标节点的 `nextHop` 是否等于中转节点。
