# certs/

系统内置信任的证书。由 `module/internal-ca.nix` 通过
`security.pki.certificateFiles` 装载进 NixOS 信任库。

## Caddy 本地 CA 根证书

两个公网节点各有一套独立的 Caddy 本地 CA（`tls internal` 自动生成），
所以根证书不同。节点上的原始位置：

    /var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt

| 文件 | 节点 | Subject | SHA-256 指纹 | 有效期 |
| --- | --- | --- | --- | --- |
| `caddy-local-root-aliyun-hk.crt` | aliyun-hk (47.83.14.140) | CN=Caddy Local Authority - 2022 ECC Root | `2A:B2:0B:18:AE:10:BA:44:35:34:CA:A2:0E:EC:F5:ED:82:A2:CE:E9:73:56:AB:55:FE:63:BE:D6:65:27:9A:2E` | 2022-07-07 → 2032-05-15 |
| `caddy-local-root-aliyun-gz.crt` | aliyun-gz (8.138.123.170) | CN=Caddy Local Authority - 2026 ECC Root | `54:1A:A2:3A:67:AE:DD:6E:64:B3:CD:D8:AE:88:E4:83:8B:D2:09:83:E2:28:09:13:AF:72:D1:99:81:94:1D:F8` | 2026-10-04 → 2036-08-12 |

## 什么时候需要更新

- 节点重装系统、或清空 `/var/lib/caddy`：Caddy 会生成全新的本地 CA，旧根失效。
- 证书已经换成 Let's Encrypt 等公共 CA：这些文件可以删掉。

## 重新导出

    ssh root@<节点> 'cat /var/lib/caddy/.local/share/caddy/pki/authorities/local/root.crt'

覆盖本目录对应文件后，核对指纹：

    openssl x509 -in certs/<文件>.crt -noout -subject -fingerprint -sha256 -dates

再用叶子证书验证整条链（应输出 `Verify return code: 0 (ok)`）：

    echo | openssl s_client -connect <公网IP>:443 -CAfile certs/<文件>.crt 2>/dev/null \
      | grep 'Verify return code'

## 客户端（非 NixOS 的系统）

`security.pki` 只影响 NixOS 节点自己。Windows / Android / OpenHarmony / macOS
各有一套独立信任库，Linux 上装好了它们并不会跟着认，需要在各自系统里各导入一次。

- **Windows**：管理员运行 `certs/windows/install-caddy-roots.ps1`（已内嵌上面两张 PEM）。
  - `-EnableFirefox`：顺带写 Firefox 企业策略 `ImportEnterpriseRoots=1`（Firefox 自带 NSS，默认不读 Windows 根证书；
    也可以在 `about:config` 手动把 `security.enterprise_roots.enabled` 设为 `true`）。
  - `-Uninstall`：删除所有 Subject 含 `Caddy Local Authority` 的根证书。
  - Chrome / Edge / Electron 应用走 Windows 根证书存储，导入后重启浏览器即可。
- **根证书有效期约 10 年，导入一次即可**；Caddy 每 12 小时轮换的是叶子证书，不影响客户端信任。

彻底免导入的办法是换成公共 CA 签发的证书（需要域名 + DNS-01 验证），
这样 Windows / 手机等所有系统开箱即信任。

