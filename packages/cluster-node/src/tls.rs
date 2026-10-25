//! 自签 TLS 证书与 rustls 配置。
//!
//! 对等节点 API 只跑 HTTP/2（ALPN=h2）。证书是每节点自签的，客户端不做
//! WebPKI 校验：链路身份由 auth.rs 的 SSH host key 签名保证，证书只负责
//! 加密与 ALPN。

use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result};
use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer};
use rustls::{ClientConfig, ServerConfig};

/// 本节点的 TLS 身份。
pub struct TlsIdentity {
    pub cert_pem: String,
    pub cert_der: CertificateDer<'static>,
    pub key_der: PrivateKeyDer<'static>,
}

impl std::fmt::Debug for TlsIdentity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TlsIdentity")
            .field(
                "fingerprint",
                &crate::auth::cert_fingerprint(self.cert_der.as_ref()),
            )
            .finish_non_exhaustive()
    }
}

/// 安装 ring provider（只安装一次；重复安装返回 false 不算错误）。
pub fn init_crypto() {
    let _ = rustls::crypto::ring::default_provider().install_default();
}

/// 优先读取配置指定的证书/私钥，否则在 state_dir/tls 下生成并复用。
pub fn load_or_generate(
    state_dir: &Path,
    node: &str,
    cert_path: Option<&Path>,
    key_path: Option<&Path>,
) -> Result<TlsIdentity> {
    let cert_path = cert_path
        .map(Path::to_path_buf)
        .unwrap_or_else(|| state_dir.join("tls/cert.pem"));
    let key_path = key_path
        .map(Path::to_path_buf)
        .unwrap_or_else(|| state_dir.join("tls/key.pem"));

    if let (Ok(cert_pem), Ok(key_pem)) = (
        std::fs::read_to_string(&cert_path),
        std::fs::read_to_string(&key_path),
    ) {
        if let Ok(identity) = from_pem(&cert_pem, &key_pem) {
            return Ok(identity);
        }
    }

    if let Some(parent) = cert_path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("创建证书目录 {} 失败", parent.display()))?;
    }
    if let Some(parent) = key_path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("创建私钥目录 {} 失败", parent.display()))?;
    }

    let certified =
        rcgen::generate_simple_self_signed(vec![node.to_string()]).context("生成自签证书失败")?;
    let cert_pem = certified.cert.pem();
    let key_pem = certified.key_pair.serialize_pem();
    std::fs::write(&cert_path, &cert_pem)
        .with_context(|| format!("写入 {}", cert_path.display()))?;
    std::fs::write(&key_path, &key_pem).with_context(|| format!("写入 {}", key_path.display()))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(&key_path, std::fs::Permissions::from_mode(0o600));
    }

    Ok(TlsIdentity {
        cert_der: certified.cert.der().clone(),
        key_der: PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(certified.key_pair.serialize_der())),
        cert_pem,
    })
}

fn from_pem(cert_pem: &str, key_pem: &str) -> Result<TlsIdentity> {
    let certs: Vec<CertificateDer<'static>> = rustls_pemfile::certs(&mut cert_pem.as_bytes())
        .collect::<std::result::Result<_, _>>()
        .context("解析证书 PEM 失败")?;
    let cert_der = certs.first().cloned().context("证书文件为空")?;
    let key_der = rustls_pemfile::private_key(&mut key_pem.as_bytes())
        .context("解析私钥 PEM 失败")?
        .context("私钥文件为空")?;
    Ok(TlsIdentity {
        cert_pem: cert_pem.to_string(),
        cert_der,
        key_der,
    })
}

/// HTTP/2 服务端配置。
pub fn server_config(identity: &TlsIdentity) -> Result<Arc<ServerConfig>> {
    let mut config = ServerConfig::builder()
        .with_no_client_auth()
        .with_single_cert(
            vec![identity.cert_der.clone()],
            identity.key_der.clone_key(),
        )
        .context("构造 rustls ServerConfig 失败")?;
    config.alpn_protocols = vec![b"h2".to_vec()];
    Ok(Arc::new(config))
}

/// HTTP/2 客户端配置：不做 WebPKI 校验，身份由请求签名保证。
pub fn client_config() -> Result<Arc<ClientConfig>> {
    let mut config = ClientConfig::builder()
        .dangerous()
        .with_custom_certificate_verifier(Arc::new(NoVerification(
            rustls::crypto::CryptoProvider::get_default()
                .cloned()
                .unwrap_or_else(|| Arc::new(rustls::crypto::ring::default_provider())),
        )))
        .with_no_client_auth();
    config.alpn_protocols = vec![b"h2".to_vec()];
    Ok(Arc::new(config))
}

#[derive(Debug)]
struct NoVerification(Arc<rustls::crypto::CryptoProvider>);

impl rustls::client::danger::ServerCertVerifier for NoVerification {
    fn verify_server_cert(
        &self,
        _end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        _server_name: &rustls::pki_types::ServerName<'_>,
        _ocsp_response: &[u8],
        _now: rustls::pki_types::UnixTime,
    ) -> std::result::Result<rustls::client::danger::ServerCertVerified, rustls::Error> {
        Ok(rustls::client::danger::ServerCertVerified::assertion())
    }

    fn verify_tls12_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &rustls::DigitallySignedStruct,
    ) -> std::result::Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        rustls::crypto::verify_tls12_signature(
            message,
            cert,
            dss,
            &self.0.signature_verification_algorithms,
        )
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &rustls::DigitallySignedStruct,
    ) -> std::result::Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        rustls::crypto::verify_tls13_signature(
            message,
            cert,
            dss,
            &self.0.signature_verification_algorithms,
        )
    }

    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        self.0.signature_verification_algorithms.supported_schemes()
    }
}
