use std::time::Duration;

use axum::http::{header, HeaderMap};
use bcrypt::DEFAULT_COST;

pub const COOKIE_NAME: &str = "homepage_session";

pub fn new_token() -> String {
    format!(
        "{}{}",
        uuid::Uuid::new_v4().simple(),
        uuid::Uuid::new_v4().simple()
    )
}

pub fn cookie_token(headers: &HeaderMap) -> Option<String> {
    let cookies = headers.get(header::COOKIE)?.to_str().ok()?;
    cookies
        .split(';')
        .filter_map(|pair| pair.trim().split_once('='))
        .find(|(name, _)| *name == COOKIE_NAME)
        .map(|(_, value)| value.trim().to_string())
        .filter(|value| !value.is_empty())
}

pub fn session_cookie(token: &str, max_age: i64, secure: bool) -> String {
    let mut cookie =
        format!("{COOKIE_NAME}={token}; Path=/; HttpOnly; SameSite=Lax; Max-Age={max_age}");
    if secure {
        cookie.push_str("; Secure");
    }
    cookie
}

pub fn clear_cookie(secure: bool) -> String {
    session_cookie(
        "",
        Duration::from_hours(12 * 30 * 24).as_secs() as i64,
        secure,
    )
}

pub async fn verify_password(password: &str, hash: &str) -> bool {
    let password = password.to_string();
    let hash = hash.to_string();
    match tokio::task::spawn_blocking(move || bcrypt::verify(password, &hash)).await {
        Ok(Ok(verified)) => verified,
        Ok(Err(err)) => {
            tracing::error!("bcrypt 校验失败: {err}");
            false
        }
        Err(err) => {
            tracing::error!("bcrypt 任务执行失败: {err}");
            false
        }
    }
}

pub fn hash_password(password: &str) -> anyhow::Result<String> {
    Ok(bcrypt::hash(password, DEFAULT_COST)?)
}
