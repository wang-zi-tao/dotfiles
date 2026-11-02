---
name: wps-linux-remote-test
description: "通过 ssh + podman 在 wangzi-kingsoft（NixOS）的 wps_debug 容器里跑 WPS 仓库的构建/编程/测试。覆盖零信任 yunshu 的登录与连接（krepo-ng 访问内网依赖它）、krepo-ng 版本升级、node 安装（glibc-217）、脚本推送范式与常见坑。当需要连接这台 Linux 机器、容器内命令报网络超时、krepo-ng 提示「访问接口失败/未检测到 node」时使用。"
version: 1.0.0
author: wps
license: MIT
platforms: [windows]
metadata:
  hermes:
    tags: [linux, ssh, podman, nixos, krepo-ng, yunshu, zerotrust, wps]
    related_skills: [wps-build-system, dotfiles]
---

# WPS Linux 远程测试环境（wangzi-kingsoft / podman wps_debug）

在这台 Linux 上跑 WPS 仓库的 krepo-ng 构建、门禁检查（jsapicheck）、CR 提交。**所有操作都在 podman 容器 `wps_debug` 里执行**，宿主机只作为跳板。

> 2026-11-02 实测环境：NixOS 6.12.60 宿主机 + Ubuntu 16.04 容器 + yunshu 2.3.10.30 + krepo-ng 3.2.38 + node v20.18.0

## 0. 快速开始（三条命令）

```powershell
# 1) 连接并确认容器在跑
ssh wangzi@wangzi-kingsoft "bash -lc 'podman ps --format \"{{.Names}} {{.Status}}\"'"

# 2) 确认零信任已连接（内网不通时 krepo-ng 一定失败）
ssh wangzi@wangzi-kingsoft "bash -lc 'podman exec wps_debug yunshu -i'"

# 3) 在容器里跑命令（工作目录 /wps/branch1/wpsmain）
ssh wangzi@wangzi-kingsoft "podman exec -w /wps/branch1/wpsmain wps_debug bash -lc 'krepo-ng --no-update jsapicheck -h'"
```

## 1. 连接链路

```
Windows 本机
  └─ ssh wangzi@wangzi-kingsoft   (NixOS 6.12.60)
       └─ podman exec wps_debug   (Ubuntu 16.04, root, 无 systemd)
            └─ /wps/branch1/wpsmain   (仓库 + krepo-ng + node)
```

### 1.1 宿主机是 nushell

远端默认 shell 是 **nushell**（不是 bash）：

### 1.2 复杂脚本：写本地 .sh → scp → bash（强烈推荐）

PowerShell → ssh → nushell → bash → podman 五层引号嵌套，**多层引号几乎必崩**（`nu::parser::unexpected_eof`、pwsh 把 `{{.Names}}` 当自己的语法）。可靠做法：

```powershell
# 1) 本地写脚本（tools.write，UTF-8 无 BOM）
#    D:\branch1\wpsmain\.temp\testrun\step.sh
# 2) 推过去再执行
scp -o ConnectTimeout=10 -q D:\branch1\wpsmain\.temp\testrun\step.sh wangzi@wangzi-kingsoft:/tmp/step.sh
ssh -o ConnectTimeout=10 wangzi@wangzi-kingsoft "bash /tmp/step.sh"
```

脚本内要传 Python 之类代码时，用 base64 避免引号：

```bash
echo <base64> | base64 -d > /tmp/x.py && python3 /tmp/x.py
```

### 1.3 容器调用规范

```bash
podman exec -w /wps/branch1/wpsmain wps_debug bash -lc '...'   # ✓ 指定工作目录 + bash -lc 拿到 PATH
podman exec -ti wps_debug bash -lc '...'                        # ✗ ssh 无 tty 会失败
```

- 容器内用户是 **root**，不需要 sudo。
- 输出里恒有 `mesg: ttyname failed: Inappropriate ioctl for device` —— **无害噪音**，可加 `| tr -d '\r'` 清理。
- 容器内**没有 systemd**（`systemctl` 报 `Failed to connect to bus: No such file or directory`），服务要手动 nohup 起。
- `/dev/net/tun` 存在（yunshu 用得上）。

## 2. 零信任网络：yunshu（krepo-ng 的内网通行证）

**这是本机最容易踩的坑**：宿主机与容器都没有直连内网的链路，访问 `gs.dig.kso.net / mirrors.wps.cn / ksogit` 全靠 **yunshu 零信任客户端**。yunshu 没连接 → krepo-ng 报 `访问接口 https://gs.dig.kso.net/... 失败（Errno 110 Connection timed out）`。

### 2.1 组件与配置

| 项 | 值 |
|---|---|
| 客户端 | `/opt/apps/yunshu/files/bin/yunshu`（`/usr/bin/yunshu` 是软链） |
| 守护进程 | `/opt/apps/yunshu/files/bin/yunshu-daemon` |
| 版本 | `yunshu -v` → `yunshu (for Linux): 2.3.10.30` |
| 配置目录 | `/opt/apps/yunshu/files/config/`（`app_config.json` + `config.db`） |
| 企业码 | **KSO**（`{"corpcode":"KSO","sp_addr":"https://kzt.kso.net"}`） |
| 登录态 | 保存在 `config.db` 的 `main_config` 表（cookies/JWT），**有效期到 2027**，正常无需重新登录 |

### 2.2 标准连接流程

```bash
# ① 启动守护进程（容器无 systemd，只能手动起；-d 表示 daemonize）
nohup /opt/apps/yunshu/files/bin/yunshu-daemon -d > /tmp/yunshu_daemon.out 2>&1 &
sleep 3 && ps -ef | grep yunshu-daemon | grep -v grep

# ② 连接内网（pa=内网 / ga=加速 / all=两者 / an=?)
yunshu -s pa

# ③ 确认状态
yunshu -i
```

成功输出：

```
# yunshu -s pa
connect pa_connect
内网已经连接成功

# yunshu -i
App版本:2.3.10.30
内网已连接,加速已断开, 已累计为您防护403天
```

### 2.3 判断「到底连没连」

**别只看 `yunshu -i` 的文字，要打真实目标**：

```bash
curl -s -o /dev/null -w "gs=%{http_code} " -m 8 https://gs.dig.kso.net/
curl -s -o /dev/null -w "nodejs=%{http_code}\n" -m 8 https://nodejs.org/dist/index.json
```

| 状态 | gs.dig.kso.net（内网） | nodejs.org（公网） | 结论 |
|---|---|---|---|
| 未连接 | `000` | `200` | 典型症状：krepo-ng 超时 |
| 已连接 | `200` | `200` | ✓ 正常 |

连接成功后 `ip route` 会多出一批走 `tun0` 的内网路由（网关 `10.251.1.1`）：

```
default via 10.13.144.1 dev enp3s0f0                  # 默认路由不变
10.7.154.185 via 10.251.1.1 dev tun0                  # 内网网段被指向 tun0
10.8.105.63  via 10.251.1.1 dev tun0
```

### 2.4 其它子命令

| 命令 | 作用 |
|---|---|
| `yunshu -l -c KSO` | 登录（**cookie 失效时才需要**，要带企业码） |
| `yunshu -s pa` / `-s ga` / `-s all` | 连接内网 / 加速 / 全部 |
| `yunshu -k pa` / `-k all` | 断开 |
| `yunshu -i` | 查状态（内网/加速连接情况） |
| `yunshu -o` | 注销 |
| `yunshu -v` | 版本 |
| `yunshu -r` | 风险检查 |
| `yunshu-daemon -d` | 后台启动守护进程 |

### 2.5 守护进程没在跑的症状

```
$ yunshu -i
后台服务未能正常运行，请重启后台服务: ipc error: connect update server failed
```

→ 回到 2.2 第 ① 步手动起 daemon。**容器重启后 daemon 不会自动起**（没有 systemd），每次进容器先查一遍。

## 3. krepo-ng 在 Linux 上的使用

### 3.1 版本与升级

| 项 | 值 |
|---|---|
| 可执行文件 | `/usr/local/bin/krepo-ng`（`#!/usr/bin/python3.9`） |
| 预装版本 | **v3.2.16 —— 没有 `jsapicheck` 子命令**（子命令只到 `check-branch`） |
| 升级 | `krepo-ng --no-update selfupdate` → **v3.2.38**（装到 `/root/.local/lib/python3.9/site-packages`） |
| PyPI 源 | `https://mirrors.wps.cn/pypi/dev/` |
| Python | 3.9.0 / pip 22.1.2 |

> 升级前必须先连 yunshu，否则 `selfupdate` 直接报 `访问接口 ... krepo-ng_version 失败（Errno 110）`。

### 3.2 常用命令（与 Windows 侧参数一致）

```bash
# 更新代码 / 切分支（第一次跑仓库必做）
krepo-ng --no-update sync                                   # 约 8 分钟，末尾可能有仓库报错，不影响主仓
krepo-ng --no-update checkout func_jsapi_cpp_checker_branch

# JSAPI 门禁检查（--commit-section 必须用真实 SHA，不支持 HEAD~1）
A=$(git -C Coding/api_bundle rev-parse HEAD~1); B=$(git -C Coding/api_bundle rev-parse HEAD)
krepo-ng --no-update jsapicheck --commit-section="$A..$B" --report-path /tmp/r.json --no-strict

# CR 提交
krepo-ng --no-update cr -m "<WPS 结构化提交信息>" --jsapi --no-browser
```

### 3.3 网络类报错对照表

| 报错 | 根因 | 处理 |
|---|---|---|
| `访问接口 https://gs.dig.kso.net/... 失败（Errno 110 Connection timed out）` | yunshu 未连接 | `yunshu -s pa` |
| `未检测到 node 环境，jsapi_cpp_checker 依赖 Node.js 运行` | 容器无 node（**krepo 不会自动装**） | 见 4.2 装 node |
| `未找到 JSAPI 门禁配置文件` | cwd 不是 wpsmain 根 | `cd /wps/branch1/wpsmain` |
| `本地api_bundle仓库...log记录中不存在commitID：HEAD~1` | `--commit-section` 不支持相对引用 | 用 `rev-parse` 展开真实 SHA |
| checkout 报「本地修改将被检出操作覆盖」 | 子仓有本地改动（常见 `Coding/core_bundle/office/newchart/CMakeLists.txt`） | `git -C Coding/core_bundle stash push` 后重试 |
| checkout 日志 `git config user.name 使用异常` | 未配 git 身份 | `git config --global user.name wangzitao; user.email wangzitao@kingsoft.com` |

## 4. 环境信息

### 4.1 软硬件清单

| 项 | 值 |
|---|---|
| 宿主机 | NixOS 6.12.60，默认 shell **nushell** |
| 容器 | `wps_debug`（podman），Ubuntu **16.04 xenial**，**glibc 2.23**，用户 root |
| 仓库 | `/wps/branch1/wpsmain`（多 bundle 子仓，`Coding/api_bundle` 独立 git） |
| krepo-ng | `/usr/local/bin/krepo-ng` v3.2.38（升级后） |
| node | 无预装 → `/opt/node20/bin/node` v20.18.0（软链到 `/usr/local/bin`） |
| python | 3.9.0 |
| 零信任 | yunshu 2.3.10.30，企业码 KSO |

### 4.2 装 node（最容易卡住的一步）

容器 glibc 只有 **2.23**（Ubuntu 16.04），**官方 node18+ 需要 glibc 2.28，装了也跑不起来**；`apt install nodejs` 只能到 4.2.6。必须用 unofficial-builds 的 glibc-217 构建：

```bash
cd /tmp
curl -fsSL -o node20.tar.gz https://unofficial-builds.nodejs.org/download/release/v20.18.0/node-v20.18.0-linux-x64-glibc-217.tar.gz
rm -rf /opt/node20 && mkdir -p /opt/node20
tar -xzf node20.tar.gz -C /opt/node20 --strip-components=1
ln -sf /opt/node20/bin/node /usr/local/bin/node
ln -sf /opt/node20/bin/npm  /usr/local/bin/npm
ln -sf /opt/node20/bin/npx  /usr/local/bin/npx
hash -r && node -v    # v20.18.0
```

**不要用 node16**：jsapi_cpp_checker 的 `--commit-section` 模式会卡在 `parse typescript file: .../et_extend_api.d.ts`，node 进程 100% CPU 跑 8 分钟以上不结束；换 node20 同一任务 **72 秒**完成。

### 4.3 备用代理（yunshu 不可用时）

容器侧有公网出口，但内网只能走 yunshu。若内网仍不通且只是要下公网文件，可用 Windows 本机代理：

```bash
curl -x http://10.13.148.196:7890 ...    # 10.13.148.196 = Windows 本机以太网 IP
```

- **不能写 `127.0.0.1`**：容器里的 `127.0.0.1` 指容器自身。
- 实测速度：直连 ~45 KB/s → 走代理 ~380 KB/s。

## 5. 常见坑速查

| 现象 | 原因 | 解决 |
|---|---|---|
| `nu::parser::unknown_flag` / `unexpected_eof` | 远端是 nushell，或 PowerShell 引号被吃 | `ssh host "bash -lc '...'"`，复杂脚本走 scp + `bash /tmp/x.sh` |
| `systemctl` 报 `Failed to connect to bus` | 容器无 systemd | 直接 `nohup ...-daemon -d &` |
| `podman exec -ti` 无反应/失败 | ssh 无 tty | 去掉 `-ti`（用 `-i` 或不带） |
| 每次输出 `mesg: ttyname failed` | 无 tty 的正常噪音 | 忽略，或 `\| tr -d '\r'` |
| `pkill -f "xxx.tar.gz"` 后整条命令静默退出 | pattern 匹配到脚本自身 | 用不匹配自身的 pattern，或先记 PID |
| 改文件后 `git diff` 显示整个文件都变了 | 用了文本模式写入把 CRLF 改成 LF | 用 Python **二进制模式** `open(p,'rb')/open(p,'wb')` 保留行尾 |
| `scp` 报 Connection timed out 但 `ssh` 能连 | 加了 `-o BatchMode=yes` 且首次连接 | 去掉 BatchMode |

## 6. 一次性环境检查脚本

```bash
#!/bin/bash
# 用法: scp 到 /tmp/env_check.sh 后 ssh host "bash /tmp/env_check.sh"
set -u
echo "== yunshu =="
podman exec wps_debug yunshu -i 2>&1 | tr -d '\r'
echo "== 内网连通 =="
podman exec wps_debug bash -lc 'curl -s -o /dev/null -w "gs=%{http_code}\n" -m 8 https://gs.dig.kso.net/'
echo "== 工具版本 =="
podman exec wps_debug bash -lc 'krepo-ng --version; node -v; python3 -V' 2>&1 | tr -d '\r'
echo "== 仓库 =="
podman exec -w /wps/branch1/wpsmain wps_debug bash -lc 'git branch --show-current; git status --short | head -5' 2>&1 | tr -d '\r'
```

## 7. 相关经验

- 在 Linux 上验证 jsapi 门禁时，**Windows 与 Linux 结果逐项一致**（同 commit + 同 diff：2947 items / 1066 error / 1881 warning，4 类 kind 计数相同），只有报告里 `cppFile.path` 分隔符不同（Windows 反斜杠 / Linux 正斜杠）。
- CR 流程与 Windows 完全一致：`krepo-ng cr -m "..." --jsapi --no-browser`；门禁不通过时页面同样显示「报告上传失败」（krepo 侧 R12 缺陷）。
