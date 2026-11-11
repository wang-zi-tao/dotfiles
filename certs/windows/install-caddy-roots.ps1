# 把各节点 Caddy 本地 CA 的根证书装入 Windows「受信任的根证书颁发机构」。
#
# 背景：NixOS 侧 module/internal-ca.nix 只把根证书装进 Linux 的 security.pki，
# Windows 用的是自己的 CryptoAPI 证书存储（Cert:\LocalMachine\Root），完全不看 Linux 的信任库，
# 所以同一套自签证书在 Windows 上仍然告警。根证书有效期约 10 年，
# 导入一次即可覆盖之后所有 12 小时轮换的叶子证书。
#
# 用法（管理员权限）：
#   powershell -ExecutionPolicy Bypass -File .\install-caddy-roots.ps1
#   powershell -ExecutionPolicy Bypass -File .\install-caddy-roots.ps1 -EnableFirefox
#   powershell -ExecutionPolicy Bypass -File .\install-caddy-roots.ps1 -Uninstall
#
# 注意：Chrome / Edge 直接读 Windows 根证书存储；Firefox 自带 NSS 库，
# 需要 -EnableFirefox（写企业策略 ImportEnterpriseRoots）或手动导入。

[CmdletBinding()]
param(
    [switch]$Uninstall,
    [switch]$EnableFirefox
)

$ErrorActionPreference = 'Stop'

$isAdmin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()
).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
    throw '需要管理员权限：请右键「以管理员身份运行」，或在管理员 PowerShell 里执行。'
}

# 与仓库 certs/caddy-local-root-aliyun-*.crt 内容一致
$pems = [ordered]@{
    'aliyun-hk (47.83.14.140) CN=Caddy Local Authority - 2022 ECC Root' = @'
-----BEGIN CERTIFICATE-----
MIIBpDCCAUqgAwIBAgIRAPpZoTyy0Ah0D7lCNGTfVekwCgYIKoZIzj0EAwIwMDEu
MCwGA1UEAxMlQ2FkZHkgTG9jYWwgQXV0aG9yaXR5IC0gMjAyMiBFQ0MgUm9vdDAe
Fw0yMjA3MDcxNTE3MDFaFw0zMjA1MTUxNTE3MDFaMDAxLjAsBgNVBAMTJUNhZGR5
IExvY2FsIEF1dGhvcml0eSAtIDIwMjIgRUNDIFJvb3QwWTATBgcqhkjOPQIBBggq
hkjOPQMBBwNCAARI2MzScKG2gWgV4e7tcLpTu+is8pgDXJpUw4M1+pTE/Q5vZ8Qz
A/KfOWu1bCikV3hBepBK/c89V634hfMlKa0Ro0UwQzAOBgNVHQ8BAf8EBAMCAQYw
EgYDVR0TAQH/BAgwBgEB/wIBATAdBgNVHQ4EFgQUk/siTBVPlGAfk+jQikLrE6Wq
IEEwCgYIKoZIzj0EAwIDSAAwRQIgTrsLlNnB4GRxLLmev2uDzqsQwaJnUhcbONux
n2r1tgoCIQDz4I+Hfu7dqWpsBq4S4fgHTnAxB98IRbzFo2CX/N8KWw==
-----END CERTIFICATE-----
'@
    'aliyun-gz (8.138.123.170) CN=Caddy Local Authority - 2026 ECC Root' = @'
-----BEGIN CERTIFICATE-----
MIIBpDCCAUqgAwIBAgIRANPPNRHKb4DuQWF5g3kZ1wcwCgYIKoZIzj0EAwIwMDEu
MCwGA1UEAxMlQ2FkZHkgTG9jYWwgQXV0aG9yaXR5IC0gMjAyNiBFQ0MgUm9vdDAe
Fw0yNjEwMDQwNjM3MjNaFw0zNjA4MTIwNjM3MjNaMDAxLjAsBgNVBAMTJUNhZGR5
IExvY2FsIEF1dGhvcml0eSAtIDIwMjYgRUNDIFJvb3QwWTATBgcqhkjOPQIBBggq
hkjOPQMBBwNCAASs8Jrn9xWzPw5JfZlAGchlykojwqreHkSTOO7su5TzTRXKTx/f
TiITRDV9XFlcTV1xMLXGM096HoKZcKUZF3k0o0UwQzAOBgNVHQ8BAf8EBAMCAQYw
EgYDVR0TAQH/BAgwBgEB/wIBATAdBgNVHQ4EFgQULemypThZpxIjjj9AON83IxGN
Q+8wCgYIKoZIzj0EAwIDSAAwRQIgaHdThEd1EH4Hzg7ogFZ7+/uFrAdwXv4EV8ma
wjSlEV4CIQD9EIMRyp9Tb53baYPNyjPvSadSaOsMioUFftp0jKHtWA==
-----END CERTIFICATE-----
'@
}

if ($Uninstall) {
    $found = Get-ChildItem Cert:\LocalMachine\Root | Where-Object { $_.Subject -like '*Caddy Local Authority*' }
    if (-not $found) {
        Write-Host '没有找到 Caddy Local Authority 根证书，无需卸载。'
        return
    }
    foreach ($cert in $found) {
        Write-Host ("移除: " + $cert.Subject)
        $cert | Remove-Item
    }
    Write-Host '完成。'
    return
}

# WinPS 5.1 的 X509Certificate2(byte[]) 只吃 DER，所以把 PEM 的 base64 体解出来再构造证书；
# 这条路比 certutil -addstore 对 PEM 的支持更确定。
$store = New-Object System.Security.Cryptography.X509Certificates.X509Store('Root', 'LocalMachine')
$store.Open([System.Security.Cryptography.X509Certificates.OpenFlags]::ReadWrite)
try {
    foreach ($name in $pems.Keys) {
        Write-Host ("安装: " + $name)
        $b64 = ($pems[$name] -replace '-----BEGIN CERTIFICATE-----', '' -replace '-----END CERTIFICATE-----', '') -replace '\s', ''
        $der = [Convert]::FromBase64String($b64)
        $cert = [System.Security.Cryptography.X509Certificates.X509Certificate2]::new($der)
        $store.Add($cert)
    }
}
finally {
    $store.Close()
}

if ($EnableFirefox) {
    $path = 'HKLM:\SOFTWARE\Policies\Mozilla\Firefox\Certificates'
    New-Item -Path $path -Force | Out-Null
    New-ItemProperty -Path $path -Name ImportEnterpriseRoots -Value 1 -PropertyType DWord -Force | Out-Null
    Write-Host '已写入 Firefox 企业策略 ImportEnterpriseRoots=1（重启 Firefox 生效）。'
}

Write-Host ''
Write-Host '已安装的 Caddy 根证书：'
Get-ChildItem Cert:\LocalMachine\Root |
    Where-Object { $_.Subject -like '*Caddy Local Authority*' } |
    Format-Table -AutoSize -Property Subject, NotAfter

Write-Host '验证（应返回 200，且不再有证书错误）：'
Write-Host '  Invoke-WebRequest https://8.138.123.170/homepage/ -UseBasicParsing -MaximumRedirection 0'
Write-Host '如需 29443 站点：Invoke-WebRequest https://8.138.123.170:29443/ -UseBasicParsing'
