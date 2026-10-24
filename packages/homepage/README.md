# homepage：个人浏览器主页

最初的三项规划：

1. sso 统一登录服务 —— **未实现**（登录逻辑内建在本服务，还没有抽成独立服务）
2. 个人浏览器主页（需要先登录） —— **已实现**
3. 给 dsh-web 做登录认证 —— **已实现**：dsh-web 通过 Caddy `forward_auth`
   复用本服务的会话，见「SSO：给 dsh-web 做登录」

设计上一直按「能演化为 SSO」的方向做：密码只存 bcrypt 哈希（sops 管理），
会话用 HttpOnly Cookie，`/api/verify` 就是给反向代理调用的鉴权端点。

## 功能

- **登录**：bcrypt 校验 sops 里的密码哈希；成功后发 30 天会话 Cookie
  （HttpOnly / SameSite=Lax / Secure）。同一来源连续失败 5 次锁 60 秒。
- **收藏**：从 Infinity 导出的 `pages.infinity` 转换成的 `bookmarks.json`
  首次启动时导入 SQLite；之后在网页里新增、编辑、删除，按分类展示。
- **搜索栏**：默认 Google，可切换 Google / Bing / 百度 / DuckDuckGo / GitHub /
  NixOS Search，记住上次选择；输入 URL 时直接跳转。
- **入口路径**：挂在 `https://<公网 IP>/homepage/`，走 443 站点的子路径，
  不再单独开端口。
- **内网入口**：不写数据库，由 Nix 模块根据 `cluster.nodes` 里各节点启用的
  服务实时生成 JSON，归类展示；每条带公开的 SVG 图标（jsDelivr selfh.st 与
  Iconify），加载失败或没配图标时回退成小圆点。
- 深色玻璃拟态界面；`/` 聚焦搜索框，`Esc` 清空筛选。

## 技术栈

- Rust 后端（axum 0.8）+ Diesel ORM（SQLite，迁移在启动时自动执行）
- SQLite 数据库（`/var/lib/homepage/homepage.db`）
- JSON 配置文件（Nix 生成到 `/etc/homepage/config.json`）
- Vue 3 + TypeScript + Vite 前端
- Nix 编译部署（`packages/homepage/default.nix`、`module/homepage.nix`）
- sops 管理登录密钥（JSON 配置里只出现密钥文件路径）
- 转换后的收藏文件 `bookmarks.json` 直接放在源码目录，作为可手工编辑的数据源

## 目录结构

```
Cargo.toml / Cargo.lock   后端依赖与锁定
src/
  main.rs                 CLI（服务、hash-password、convert-infinity）与启动流程
  config.rs               JSON 配置（camelCase）
  api.rs                  路由、鉴权提取器、登录节流、错误响应
  auth.rs                 Cookie、bcrypt、会话 token
  db.rs                   Diesel 连接池与全部数据库操作
  schema.rs               Diesel table! 宏
  infinity.rs             pages.infinity → 简单 JSON 的转换器
  seed.rs                 初始收藏 JSON 的结构
migrations/               Diesel 迁移（启动时自动执行）
bookmarks.json            收藏数据源（已转换，直接提交，可手工编辑）
web/                      Vue 前端（package-lock.json 已提交）
default.nix               打包：Rust + 前端 + 数据文件
README.md                 本文件
```

## 数据流

```
pages.infinity --convert-infinity--> bookmarks.json --首次启动且库为空--> homepage.db
```

现在 `bookmarks.json` 是唯一的数据源，原 `pages.infinity` 已从仓库删除。要重新
从 Infinity 导出转换（**会覆盖手工修改，谨慎**），把 `pages.infinity` 放回本目录后：

```bash
cd packages/homepage
cargo run -- convert-infinity pages.infinity -o bookmarks.json
```

转换规则：顶层散落站点进「常用」；顶层文件夹用文件夹名作为分类（空名回退
「收藏夹」，重名加「 2」后缀）；只接受 http(s) 链接（`infinity://` 设置页跳
过）；按 URL 去重；图标取 Infinity 的 CDN 地址。

> 说明：原始导出里两个顶层文件夹的 `name` 字面都是「文件夹」（长度 7 和 5），
> 所以分类显示为「文件夹」和「文件夹 2」。这是源数据本来的名字；想改就直接
> 编辑 `bookmarks.json`，或直接在网页里改每条的「分类」。
>
> `bookmarks.json` 只在**数据库为空**时导入。若想让它重新生效，需要先停服务、
> 删掉 `/var/lib/homepage/homepage.db`（会丢失网页里做过的增删改），再启动。

## 登录与密钥

密码哈希存在 sops 的 `homepage/password-hash` 键（`secrets/aliyun-gz.yaml` 的
嵌套结构 `homepage: password-hash:`），值复用 `dsh-web/password-hash`，即与
dsh-web 同一个密码。

`dsh-web` 的哈希是 `caddy hash-password` 生成的（标准 bcrypt，`$2a$14$`），
不是 Caddy 私有格式；后端用 Rust `bcrypt` crate 校验，兼容 `$2a$` / `$2b$` /
`$2y$`，所以 Caddy 和本项目的 `hash-password` 生成的哈希可以互换（已实测）。

生成/轮换哈希：

```bash
# 本地或服务器上都可以；从 stdin 读密码
printf '%s' '你的新密码' | ./result/bin/homepage hash-password
# 开发时：
printf '%s' '你的新密码' | cargo run --quiet -- hash-password
```

把输出写回 sops（不要提交明文）：

```bash
sops edit secrets/aliyun-gz.yaml
# 改成：
# homepage:
#   password-hash: $2b$12$...
```

后端每次登录都会重新读这个文件，所以 `sops edit` 之后重新部署（`nixos-rebuild`
会重新渲染 secret）即可生效，不需要额外重启服务；`restartUnits` 只是兜底。

## SSO：给 dsh-web 做登录

homepage 是会话的唯一签发者，dsh-web 用 Caddy 的 `forward_auth` 向它询问每个
请求是否已登录（`module/dsh-web.nix`）：

```
forward_auth 127.0.0.1:18081 {
  uri /api/verify
  header_up X-Original-URL https://{http.request.hostport}{http.request.uri}
}
```

- `/api/verify` 校验 Cookie：已登录返回 200（带 `x-auth-user` 头）放行；未登录
  返回 302 到 `{publicBaseUrl}/homepage/?next=<原始 URL>`，前端登录成功后自动
  跳回原地址。
- 这个 302 **必须是绝对地址**（配置项 `publicBaseUrl`）。如果只给相对路径
  `/homepage/`，浏览器会把它解析到 dsh-web 自己的端口（`:29443/homepage/`）上，
  请求又被 `forward_auth` 拦下，形成无限重定向。
- Cookie 是 host-only + `Path=/`，同一主机的不同端口天然共享，所以在
  `https://8.138.123.170/homepage/` 登录一次，就能访问
  `https://8.138.123.170:29443` 的 dsh-web。
- dsh-web 自带的 launch token 那一层仍然存在：首次需要用 `dsh-web-login-url`
  从日志里取一次带 token 的 URL，浏览器打开换 dsh 自己的 30 天 Cookie；之后
  重启也不受影响。
- `forward_auth` 覆盖该站点的所有请求（包括静态资源与 WebSocket 之外的部分），
  这是有意的：dsh 的静态资源本身不认证。代价是 homepage 挂掉时，依赖它的站点
  也无法登录。

## 配置

由 `module/homepage.nix` 用 `pkgs.formats.json` 生成到
`/etc/homepage/config.json`，结构（camelCase）：

```json
{
  "title": "我的主页",
  "listen": "127.0.0.1",
  "port": 18081,
  "basePath": "/homepage",
  "publicBaseUrl": "https://8.138.123.170",
  "database": "/var/lib/homepage/homepage.db",
  "staticDir": "/nix/store/.../share/homepage/web",
  "secureCookies": true,
  "auth": {
    "username": "wangzi",
    "passwordHashFile": "/run/secrets/homepage/password-hash",
    "sessionDays": 30
  },
  "seed": { "file": "/nix/store/.../share/homepage/bookmarks.json" },
  "searchEngines": [
    { "id": "google", "name": "Google", "url": "https://www.google.com/search?q={query}" }
  ],
  "defaultSearchEngine": "google",
  "intranet": [
    {
      "name": "aliyun-hk",
      "entries": [
        {
          "name": "Nextcloud",
          "url": "https://47.83.14.140/nextcloud/",
          "icon": "https://cdn.jsdelivr.net/gh/selfhst/icons/svg/nextcloud.svg"
        }
      ]
    }
  ]
}
```

内网入口的分组来自 `cluster.nodes`：只收录有 `publicIp` 的节点，按节点启用
的服务（Nextcloud / OneDev / WebSSH / Atuin / Grafana / Xpra / dsh-web）生成。

## HTTP API

| 方法 | 路径 | 需要登录 | 说明 |
| --- | --- | --- | --- |
| GET | `/api/health` | 否 | 健康检查 |
| GET | `/api/meta` | 否 | 标题、搜索引擎、默认引擎 |
| GET | `/api/session` | 否 | 当前会话（401 以外都返回 200） |
| POST | `/api/auth/login` | 否 | `{username,password}`，成功 Set-Cookie |
| POST | `/api/auth/logout` | 是 | 删除会话并清 Cookie |
| GET | `/api/bookmarks` | 是 | 全部收藏（按分类/位置排序） |
| POST | `/api/bookmarks` | 是 | 新增，返回 201 |
| PUT | `/api/bookmarks/{id}` | 是 | 修改 |
| DELETE | `/api/bookmarks/{id}` | 是 | 删除，返回 204 |
| GET | `/api/intranet` | 是 | 内网入口分组 |
| GET | `/api/verify` | 否 | 给 Caddy `forward_auth` 用：已登录 200，否则 302 到登录页 |

## 部署（aliyun-gz）

`module/cluster.nix` 里 `aliyun-gz` 已启用：

```nix
homepage.enable = true;   # bindPort = 18081，只绑 127.0.0.1
```

访问地址：`https://8.138.123.170/homepage/`。Caddy 在已有的
`https://8.138.123.170` 站点（`module/network.nix` 的 443 站点，`tls internal`
自签证书）里加了一条 `/homepage/*` 路由：`uri strip_prefix /homepage` 后反代到
`127.0.0.1:18081`；`/homepage` 会 308 跳到 `/homepage/`。首次打开仍有证书
告警，属预期。

部署命令（任选其一）：

```bash
./install.bash nixos-remote aliyun-gz
./install.bash deploy aliyun-gz
```

部署后：

```bash
systemctl status homepage --no-pager
journalctl -u homepage -f
```

## 本地开发

```bash
# 后端：需要一份配置；staticDir 可先指向已构建的 web/dist
cd packages/homepage
cargo run -- --config /tmp/homepage-dev.json

# 前端：npm run dev 会把 /api 代理到 127.0.0.1:18081
cd packages/homepage/web
npm install --include=dev
npm run dev
```

构建与检查：

```bash
cd packages/homepage
cargo check
cd web && npm run build && npm run typecheck
cd ../.. && nix build .#homepage
```

## 已知取舍

- 没有注册、找回密码、多用户：单用户个人主页，改密码就是改 sops。
- 收藏分类只按顺序展示，不支持拖拽排序；排序由数据库 `position` 决定。
- 内网入口是「依据 Nix 配置生成」的静态数据，不能从网页编辑。
- 首屏所有收藏一次返回，个人规模（几十条）足够。
- SSO 目前只有 dsh-web 一个接入方；homepage 与 dsh-web 必须同机（forward_auth
  走 127.0.0.1），多机部署需要把鉴权端点和 Cookie 域一起设计。
