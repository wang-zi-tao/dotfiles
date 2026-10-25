//! 节点身份与请求签名。
//!
//! 每个节点用本机的 SSH host key（ed25519）对请求签名，对端用配置里的
//! hostPubKey 固定校验。TLS 只是自签证书提供的加密通道，真正的身份
//! 由这里的签名保证，因此中间人无法仅凭伪造证书冒充节点。

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{bail, Context, Result};
use base64::Engine as _;
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use rand::RngCore;
use sha2::{Digest, Sha256};
use ssh_key::public::KeyData;
use ssh_key::{Algorithm, HashAlg, PrivateKey, PublicKey};

/// 签名覆盖的协议版本，改动 canonical 串时同步升级。
pub const AUTH_VERSION: &str = "cluster-node-auth-v1";
const B64: base64::engine::general_purpose::GeneralPurpose =
    base64::engine::general_purpose::STANDARD;

pub fn now_unix() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

pub fn random_nonce() -> String {
    let mut buf = [0u8; 16];
    OsRng.fill_bytes(&mut buf);
    hex::encode(buf)
}

fn ed25519_public_bytes(openssh: &str) -> Result<[u8; 32]> {
    let key = PublicKey::from_openssh(openssh.trim()).context("不是有效的 OpenSSH 公钥")?;
    match key.key_data() {
        KeyData::Ed25519(k) => Ok(k.0),
        _ => bail!("只支持 ed25519 host key"),
    }
}

pub fn parse_ed25519_public(openssh: &str) -> Result<VerifyingKey> {
    let bytes = ed25519_public_bytes(openssh)?;
    VerifyingKey::from_bytes(&bytes).context("无效的 ed25519 公钥")
}

/// 形如 SHA256:xxxx 的指纹，与 ssh-keygen -lf 一致。
pub fn public_key_fingerprint(openssh: &str) -> Result<String> {
    let key = PublicKey::from_openssh(openssh.trim()).context("不是有效的 OpenSSH 公钥")?;
    Ok(key.fingerprint(HashAlg::Sha256).to_string())
}

fn signing_key_from_private(private: &PrivateKey) -> Result<SigningKey> {
    match private.key_data() {
        ssh_key::private::KeypairData::Ed25519(kp) => {
            Ok(SigningKey::from_bytes(&kp.private.to_bytes()))
        }
        _ => bail!(
            "只支持 ed25519 host key（{}）",
            private.algorithm().as_str()
        ),
    }
}

/// 本节点身份。
#[derive(Clone)]
pub struct Identity {
    pub node: String,
    signing: SigningKey,
    pub public_openssh: String,
    pub source: PathBuf,
    pub generated: bool,
}

impl std::fmt::Debug for Identity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Identity")
            .field("node", &self.node)
            .field("source", &self.source)
            .field("generated", &self.generated)
            .finish_non_exhaustive()
    }
}

impl Identity {
    /// 读取 SSH host key；不存在时在 state_dir 里生成一把临时 key 并落盘。
    pub fn load_or_generate(node: &str, key_path: &Path, state_dir: &Path) -> Result<Self> {
        if let Ok(text) = std::fs::read_to_string(key_path) {
            let private = PrivateKey::from_openssh(&text)
                .with_context(|| format!("解析 SSH host key {} 失败", key_path.display()))?;
            if private.is_encrypted() {
                bail!(
                    "SSH host key {} 有密码保护，无法自动使用",
                    key_path.display()
                );
            }
            let signing = signing_key_from_private(&private)?;
            let public_openssh = private.public_key().to_openssh()?;
            return Ok(Self {
                node: node.to_string(),
                signing,
                public_openssh,
                source: key_path.to_path_buf(),
                generated: false,
            });
        }

        std::fs::create_dir_all(state_dir)
            .with_context(|| format!("创建状态目录 {} 失败", state_dir.display()))?;
        let path = state_dir.join("ssh_host_ed25519_key");
        let private = match std::fs::read_to_string(&path) {
            Ok(text) => PrivateKey::from_openssh(&text)
                .with_context(|| format!("解析 {}", path.display()))?,
            Err(_) => {
                let key = PrivateKey::random(&mut OsRng, Algorithm::Ed25519)
                    .context("生成 ed25519 host key 失败")?;
                std::fs::write(&path, key.to_openssh(ssh_key::LineEnding::LF)?.as_bytes())
                    .with_context(|| format!("写入 {}", path.display()))?;
                #[cfg(unix)]
                {
                    use std::os::unix::fs::PermissionsExt;
                    let _ = std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600));
                }
                key
            }
        };
        let signing = signing_key_from_private(&private)?;
        let public_openssh = private.public_key().to_openssh()?;
        Ok(Self {
            node: node.to_string(),
            signing,
            public_openssh,
            source: path,
            generated: true,
        })
    }

    pub fn fingerprint(&self) -> String {
        PublicKey::from_openssh(self.public_openssh.trim())
            .map(|k| k.fingerprint(HashAlg::Sha256).to_string())
            .unwrap_or_else(|_| "<invalid>".into())
    }

    pub fn sign(&self, method: &str, path: &str, time: u64, nonce: &str) -> String {
        let msg = canonical(method, path, &self.node, time, nonce);
        B64.encode(self.signing.sign(msg.as_bytes()).to_bytes())
    }

    pub fn headers(&self, method: &str, path: &str) -> AuthHeaders {
        let time = now_unix();
        let nonce = random_nonce();
        let signature = self.sign(method, path, time, &nonce);
        AuthHeaders {
            node: self.node.clone(),
            time,
            nonce,
            signature,
        }
    }
}

/// 参与签名的规范化字符串。
pub fn canonical(method: &str, path: &str, node: &str, time: u64, nonce: &str) -> String {
    format!(
        "{AUTH_VERSION}\n{}\n{}\n{}\n{}\n{}",
        method.to_ascii_uppercase(),
        path,
        node,
        time,
        nonce
    )
}

/// 请求头里的认证信息。
#[derive(Debug, Clone)]
pub struct AuthHeaders {
    pub node: String,
    pub time: u64,
    pub nonce: String,
    pub signature: String,
}

impl AuthHeaders {
    pub fn from_parts(
        node: impl Into<String>,
        time: impl Into<String>,
        nonce: impl Into<String>,
        signature: impl Into<String>,
    ) -> Result<Self> {
        let node = node.into();
        let time: u64 = time
            .into()
            .parse()
            .context("x-cluster-time 不是整数时间戳")?;
        Ok(Self {
            node,
            time,
            nonce: nonce.into(),
            signature: signature.into(),
        })
    }
}

/// 校验签名 + 时间窗口 + nonce 重放。
pub fn verify(
    public_openssh: &str,
    headers: &AuthHeaders,
    method: &str,
    path: &str,
    skew_secs: u64,
    replay: &mut ReplayCache,
) -> Result<()> {
    let now = now_unix();
    let skew = now.abs_diff(headers.time);
    if skew > skew_secs {
        bail!("请求时间戳偏差 {skew}s 超过允许的 {skew_secs}s");
    }
    let key = parse_ed25519_public(public_openssh)?;
    let raw = B64
        .decode(headers.signature.trim())
        .context("signature 不是合法 base64")?;
    let sig_bytes: [u8; 64] = raw
        .as_slice()
        .try_into()
        .map_err(|_| anyhow::anyhow!("signature 长度不是 64 字节"))?;
    let sig = Signature::from_bytes(&sig_bytes);
    let msg = canonical(method, path, &headers.node, headers.time, &headers.nonce);
    key.verify(msg.as_bytes(), &sig).context("签名校验失败")?;
    if !replay.accept(&headers.nonce) {
        bail!("nonce {} 重放", headers.nonce);
    }
    Ok(())
}

/// 简单的 nonce 重放缓存。
#[derive(Debug)]
pub struct ReplayCache {
    window: Duration,
    seen: HashMap<String, Instant>,
}

impl ReplayCache {
    pub fn new(window: Duration) -> Self {
        Self {
            window,
            seen: HashMap::new(),
        }
    }

    /// 第一次见到返回 true，窗口内重复返回 false。
    pub fn accept(&mut self, nonce: &str) -> bool {
        let now = Instant::now();
        self.seen
            .retain(|_, at| now.duration_since(*at) < self.window);
        if self.seen.contains_key(nonce) {
            return false;
        }
        self.seen.insert(nonce.to_string(), now);
        true
    }
}

/// FNV-1a，仅用于日志短 id，不做安全用途。
pub fn short_hash(data: &str) -> String {
    let mut hash: u64 = 0xcbf29ce484222325;
    for b in data.as_bytes() {
        hash ^= u64::from(*b);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{:016x}", hash)
}

/// TLS 证书指纹（SHA-256），只用于日志对照。
pub fn cert_fingerprint(der: &[u8]) -> String {
    let digest = Sha256::digest(der);
    hex::encode(digest)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn identity() -> Identity {
        let key = PrivateKey::random(&mut OsRng, Algorithm::Ed25519).unwrap();
        let signing = signing_key_from_private(&key).unwrap();
        Identity {
            node: "n1".into(),
            signing,
            public_openssh: key.public_key().to_openssh().unwrap(),
            source: PathBuf::from("/tmp/test"),
            generated: true,
        }
    }

    #[test]
    fn signs_and_verifies() {
        let id = identity();
        let headers = id.headers("GET", "/v1/graph");
        let mut replay = ReplayCache::new(Duration::from_secs(60));
        verify(
            &id.public_openssh,
            &headers,
            "GET",
            "/v1/graph",
            60,
            &mut replay,
        )
        .unwrap();
        // 重放同一 nonce 被拒绝
        assert!(verify(
            &id.public_openssh,
            &headers,
            "GET",
            "/v1/graph",
            60,
            &mut replay,
        )
        .is_err());
    }

    #[test]
    fn rejects_tampered_path() {
        let id = identity();
        let headers = id.headers("GET", "/v1/graph");
        let mut replay = ReplayCache::new(Duration::from_secs(60));
        assert!(verify(
            &id.public_openssh,
            &headers,
            "GET",
            "/v1/routes",
            60,
            &mut replay,
        )
        .is_err());
    }

    #[test]
    fn rejects_stale_timestamp() {
        let id = identity();
        let time = now_unix() - 3600;
        let nonce = random_nonce();
        let headers = AuthHeaders {
            node: id.node.clone(),
            time,
            nonce: nonce.clone(),
            signature: id.sign("GET", "/v1/ping", time, &nonce),
        };
        let mut replay = ReplayCache::new(Duration::from_secs(60));
        assert!(verify(
            &id.public_openssh,
            &headers,
            "GET",
            "/v1/ping",
            120,
            &mut replay,
        )
        .is_err());
    }

    #[test]
    fn fingerprint_matches_ssh_keygen_format() {
        let id = identity();
        let fp = public_key_fingerprint(&id.public_openssh).unwrap();
        assert!(fp.starts_with("SHA256:"), "{fp}");
    }
}
