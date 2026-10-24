use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::{Duration, Instant};

use axum::extract::{FromRequestParts, Path, Query, State};
use axum::http::request::Parts;
use axum::http::{header, HeaderMap, HeaderValue, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post, put};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use serde_json::json;
use tower_http::services::{ServeDir, ServeFile};
use tower_http::set_header::SetResponseHeaderLayer;
use tower_http::trace::TraceLayer;
use url::Url;

use crate::auth;
use crate::config::{Config, IntranetGroup, SearchEngine};
use crate::db::{self, Bookmark, DEFAULT_CATEGORY};

const MAX_FAILED_ATTEMPTS: u32 = 5;
const BLOCK_DURATION: Duration = Duration::from_secs(60);

#[derive(Clone)]
pub struct AppState {
    pub config: Arc<Config>,
    pub db: db::DbPool,
    pub throttle: Arc<Mutex<HashMap<String, Attempt>>>,
}

#[derive(Debug, Default)]
pub struct Attempt {
    failures: u32,
    blocked_until: Option<Instant>,
}

#[derive(Debug)]
pub struct AppError {
    pub status: StatusCode,
    pub message: String,
}

impl AppError {
    pub fn new(status: StatusCode, message: impl Into<String>) -> Self {
        Self {
            status,
            message: message.into(),
        }
    }

    pub fn bad_request(message: impl Into<String>) -> Self {
        Self::new(StatusCode::BAD_REQUEST, message)
    }

    pub fn unauthorized(message: impl Into<String>) -> Self {
        Self::new(StatusCode::UNAUTHORIZED, message)
    }

    pub fn not_found(message: impl Into<String>) -> Self {
        Self::new(StatusCode::NOT_FOUND, message)
    }

    pub fn internal(error: impl std::fmt::Display) -> Self {
        tracing::error!("内部错误: {error}");
        Self::new(StatusCode::INTERNAL_SERVER_ERROR, "服务器内部错误")
    }
}

impl std::fmt::Display for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.message, self.status)
    }
}

impl std::error::Error for AppError {}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        (self.status, Json(json!({ "error": self.message }))).into_response()
    }
}

impl From<diesel::result::Error> for AppError {
    fn from(err: diesel::result::Error) -> Self {
        AppError::internal(format!("数据库错误: {err}"))
    }
}

pub struct AuthUser {
    #[allow(dead_code)]
    pub username: String,
}

impl FromRequestParts<AppState> for AuthUser {
    type Rejection = AppError;

    async fn from_request_parts(
        parts: &mut Parts,
        state: &AppState,
    ) -> Result<Self, Self::Rejection> {
        let token =
            auth::cookie_token(&parts.headers).ok_or_else(|| AppError::unauthorized("请先登录"))?;
        let mut conn = db_conn(state)?;
        let username = db::session_user(&mut conn, &token)?
            .ok_or_else(|| AppError::unauthorized("登录已过期，请重新登录"))?;
        Ok(AuthUser { username })
    }
}

fn db_conn(state: &AppState) -> Result<db::DbConn, AppError> {
    state
        .db
        .get()
        .map_err(|err| AppError::internal(format!("获取数据库连接失败: {err}")))
}

fn lock_throttle(state: &AppState) -> MutexGuard<'_, HashMap<String, Attempt>> {
    state
        .throttle
        .lock()
        .unwrap_or_else(|err| err.into_inner())
}

fn client_ip(headers: &HeaderMap) -> String {
    headers
        .get("x-forwarded-for")
        .and_then(|value| value.to_str().ok())
        .and_then(|value| value.split(',').next())
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .unwrap_or("unknown")
        .to_string()
}

fn check_throttle(state: &AppState, ip: &str) -> Result<(), AppError> {
    let attempts = lock_throttle(state);
    if let Some(attempt) = attempts.get(ip) {
        if let Some(until) = attempt.blocked_until {
            if until > Instant::now() {
                let remaining = until.saturating_duration_since(Instant::now()).as_secs() + 1;
                return Err(AppError::new(
                    StatusCode::TOO_MANY_REQUESTS,
                    format!("尝试次数过多，请 {remaining} 秒后再试"),
                ));
            }
        }
    }
    Ok(())
}

fn record_failure(state: &AppState, ip: &str) {
    let mut attempts = lock_throttle(state);
    if attempts.len() > 4096 {
        let now = Instant::now();
        attempts.retain(|_, attempt| attempt.blocked_until.map(|t| t > now).unwrap_or(attempt.failures > 0));
    }
    let attempt = attempts.entry(ip.to_string()).or_default();
    if attempt.blocked_until.map(|until| until <= Instant::now()).unwrap_or(false) {
        attempt.blocked_until = None;
        attempt.failures = 0;
    }
    attempt.failures += 1;
    if attempt.failures >= MAX_FAILED_ATTEMPTS {
        attempt.failures = 0;
        attempt.blocked_until = Some(Instant::now() + BLOCK_DURATION);
    }
}

fn clear_failures(state: &AppState, ip: &str) {
    lock_throttle(state).remove(ip);
}

pub fn router(state: AppState, static_dir: Option<PathBuf>) -> Router {
    let api = Router::new()
        .route("/health", get(health))
        .route("/meta", get(meta))
        .route("/session", get(session))
        .route("/auth/login", post(login))
        .route("/auth/logout", post(logout))
        .route("/bookmarks", get(list_bookmarks).post(create_bookmark))
        .route(
            "/bookmarks/{id}",
            put(update_bookmark).delete(delete_bookmark),
        )
        .route("/intranet", get(intranet))
        .route("/verify", get(verify_session))
        .fallback(api_not_found)
        .with_state(state);

    let app = Router::new().nest("/api", api).layer(
        SetResponseHeaderLayer::overriding(
            header::X_CONTENT_TYPE_OPTIONS,
            HeaderValue::from_static("nosniff"),
        ),
    )
    .layer(SetResponseHeaderLayer::overriding(
        header::X_FRAME_OPTIONS,
        HeaderValue::from_static("DENY"),
    ))
    .layer(SetResponseHeaderLayer::overriding(
        header::REFERRER_POLICY,
        HeaderValue::from_static("same-origin"),
    ))
    .layer(TraceLayer::new_for_http());

    match static_dir {
        Some(dir) if dir.join("index.html").is_file() => {
            let index = dir.join("index.html");
            app.fallback_service(ServeDir::new(dir).not_found_service(ServeFile::new(index)))
        }
        Some(dir) => {
            tracing::warn!("静态资源目录缺少 index.html: {}", dir.display());
            app.fallback(missing_frontend)
        }
        None => app.fallback(missing_frontend),
    }
}

async fn api_not_found() -> AppError {
    AppError::not_found("接口不存在")
}

async fn missing_frontend() -> (StatusCode, &'static str) {
    (
        StatusCode::SERVICE_UNAVAILABLE,
        "前端资源未找到，请确认 staticDir 配置",
    )
}

async fn health() -> &'static str {
    "ok"
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct MetaResponse {
    title: String,
    search_engines: Vec<SearchEngine>,
    default_search_engine: Option<String>,
}

async fn meta(State(state): State<AppState>) -> Json<MetaResponse> {
    Json(MetaResponse {
        title: state.config.title.clone(),
        search_engines: state.config.search_engines.clone(),
        default_search_engine: state.config.default_search_engine.clone(),
    })
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct SessionResponse {
    authenticated: bool,
    username: Option<String>,
}

async fn session(State(state): State<AppState>, headers: HeaderMap) -> Json<SessionResponse> {
    let token = auth::cookie_token(&headers);
    let username = token.and_then(|token| {
        let mut conn = db_conn(&state).ok()?;
        db::session_user(&mut conn, &token).ok().flatten()
    });
    Json(SessionResponse {
        authenticated: username.is_some(),
        username,
    })
}

#[derive(Deserialize)]
struct LoginRequest {
    username: String,
    password: String,
}

#[derive(Serialize)]
struct LoginResponse {
    username: String,
}

async fn login(
    State(state): State<AppState>,
    headers: HeaderMap,
    Json(payload): Json<LoginRequest>,
) -> Result<Response, AppError> {
    let ip = client_ip(&headers);
    check_throttle(&state, &ip)?;

    let username_ok = payload.username.trim() == state.config.auth.username;
    let hash = state.config.password_hash().map_err(AppError::internal)?;
    let verified = auth::verify_password(&payload.password, &hash).await;
    if !username_ok || !verified {
        record_failure(&state, &ip);
        return Err(AppError::unauthorized("用户名或密码错误"));
    }
    clear_failures(&state, &ip);

    let token = auth::new_token();
    let session_days = state.config.auth.session_days.max(1) as i64;
    let ttl = session_days * 24 * 60 * 60;
    {
        let mut conn = db_conn(&state)?;
        db::create_session(&mut conn, &token, &state.config.auth.username, ttl)?;
        db::purge_expired_sessions(&mut conn)?;
    }

    let cookie = auth::session_cookie(&token, ttl, state.config.secure_cookies);
    let cookie = HeaderValue::from_str(&cookie).map_err(AppError::internal)?;
    let mut response = Json(LoginResponse {
        username: state.config.auth.username.clone(),
    })
    .into_response();
    response.headers_mut().insert(header::SET_COOKIE, cookie);
    Ok(response)
}

async fn logout(
    State(state): State<AppState>,
    headers: HeaderMap,
) -> Result<Response, AppError> {
    if let Some(token) = auth::cookie_token(&headers) {
        let mut conn = db_conn(&state)?;
        db::delete_session(&mut conn, &token)?;
    }
    let cookie = HeaderValue::from_str(&auth::clear_cookie(state.config.secure_cookies))
        .map_err(AppError::internal)?;
    let mut response = StatusCode::NO_CONTENT.into_response();
    response.headers_mut().insert(header::SET_COOKIE, cookie);
    Ok(response)
}

#[derive(Deserialize)]
struct VerifyQuery {
    #[serde(default)]
    next: Option<String>,
}

/// 供 Caddy forward_auth 调用的会话校验端点。
///
/// * 已登录：200，并在 X-Auth-User 里带上用户名；
/// * 未登录：302 跳到主页登录页，登录后按 next 回到原来的地址。
///
/// Caddy 侧用 header_up X-Original-URL 传入原始完整 URL（forward_auth 会
/// 把请求重写到 /api/verify，所以原始地址只能靠头/查询串带进来）。
async fn verify_session(
    State(state): State<AppState>,
    headers: HeaderMap,
    Query(query): Query<VerifyQuery>,
) -> Response {
    let token = auth::cookie_token(&headers);
    let username = token.and_then(|token| {
        let mut conn = db_conn(&state).ok()?;
        db::session_user(&mut conn, &token).ok().flatten()
    });

    let mut response = if let Some(username) = username {
        let mut response = StatusCode::OK.into_response();
        if let Ok(value) = HeaderValue::from_str(&username) {
            response.headers_mut().insert("x-auth-user", value);
        }
        response
    } else {
        let original = headers
            .get("x-original-url")
            .and_then(|value| value.to_str().ok())
            .map(str::to_string)
            .or(query.next);
        let login_page = login_page_url(&state.config);
        let location = match original.filter(|url| is_http_url(url)) {
            Some(url) => {
                let encoded: String = url::form_urlencoded::byte_serialize(url.as_bytes()).collect();
                format!("{login_page}?next={encoded}")
            }
            None => login_page,
        };
        let mut response = StatusCode::FOUND.into_response();
        if let Ok(value) = HeaderValue::from_str(&location) {
            response.headers_mut().insert(header::LOCATION, value);
        }
        response
    };
    response
        .headers_mut()
        .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-store"));
    response
}

/// 登录页的绝对地址：`{publicBaseUrl}{basePath}/`。
///
/// 必须带公网根地址：forward_auth 跑在别的站点上，相对路径会被解析回那个站点
/// 自己的端口，导致登录页请求又被 forward_auth 拦下，无限 302。
fn login_page_url(config: &crate::config::Config) -> String {
    let path = format!("{}/", config.base_path.trim_end_matches('/'));
    config
        .public_base_url
        .as_deref()
        .map(str::trim)
        .filter(|origin| !origin.is_empty())
        .map(|origin| format!("{}{}", origin.trim_end_matches('/'), path))
        .unwrap_or(path)
}

fn is_http_url(raw: &str) -> bool {
    Url::parse(raw)
        .map(|url| matches!(url.scheme(), "http" | "https") && url.host_str().is_some())
        .unwrap_or(false)
}

async fn list_bookmarks(
    State(state): State<AppState>,
    _user: AuthUser,
) -> Result<Json<Vec<Bookmark>>, AppError> {
    let mut conn = db_conn(&state)?;
    Ok(Json(db::list_bookmarks(&mut conn)?))
}

async fn intranet(
    State(state): State<AppState>,
    _user: AuthUser,
) -> Json<Vec<IntranetGroup>> {
    Json(state.config.intranet.clone())
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct BookmarkInput {
    #[serde(default)]
    name: String,
    url: String,
    #[serde(default)]
    category: Option<String>,
    #[serde(default)]
    icon: Option<String>,
}

struct CleanBookmark {
    name: String,
    url: String,
    category: String,
    icon: Option<String>,
}

fn clean_bookmark(input: BookmarkInput) -> Result<CleanBookmark, AppError> {
    let url = normalize_url(&input.url)?;
    let name = clean_name(&input.name, &url);
    let category = clean_category(input.category);
    let icon = clean_icon(input.icon)?;
    Ok(CleanBookmark {
        name,
        url,
        category,
        icon,
    })
}

fn normalize_url(raw: &str) -> Result<String, AppError> {
    let raw = raw.trim();
    if raw.is_empty() {
        return Err(AppError::bad_request("网址不能为空"));
    }
    if raw.len() > 2000 {
        return Err(AppError::bad_request("网址过长"));
    }
    let candidate = if raw.contains("://") {
        raw.to_string()
    } else {
        format!("https://{raw}")
    };
    let parsed = Url::parse(&candidate).map_err(|_| AppError::bad_request("网址格式不正确"))?;
    if !matches!(parsed.scheme(), "http" | "https") {
        return Err(AppError::bad_request("只支持 http/https 网址"));
    }
    if parsed.host_str().is_none() {
        return Err(AppError::bad_request("网址缺少主机名"));
    }
    Ok(parsed.to_string())
}

fn clean_name(raw: &str, url: &str) -> String {
    let name = raw.split_whitespace().collect::<Vec<_>>().join(" ");
    if name.is_empty() {
        Url::parse(url)
            .ok()
            .and_then(|parsed| parsed.host_str().map(str::to_string))
            .unwrap_or_else(|| url.to_string())
    } else {
        name.chars().take(120).collect()
    }
}

fn clean_category(raw: Option<String>) -> String {
    let category = raw
        .unwrap_or_default()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    if category.is_empty() {
        DEFAULT_CATEGORY.to_string()
    } else {
        category.chars().take(60).collect()
    }
}

fn clean_icon(raw: Option<String>) -> Result<Option<String>, AppError> {
    let Some(raw) = raw else {
        return Ok(None);
    };
    let raw = raw.trim();
    if raw.is_empty() {
        return Ok(None);
    }
    let parsed = Url::parse(raw).map_err(|_| AppError::bad_request("图标网址格式不正确"))?;
    if !matches!(parsed.scheme(), "http" | "https") {
        return Err(AppError::bad_request("图标只支持 http/https 网址"));
    }
    Ok(Some(parsed.to_string()))
}

async fn create_bookmark(
    State(state): State<AppState>,
    _user: AuthUser,
    Json(input): Json<BookmarkInput>,
) -> Result<(StatusCode, Json<Bookmark>), AppError> {
    let bookmark = clean_bookmark(input)?;
    let mut conn = db_conn(&state)?;
    let created = db::create_bookmark(
        &mut conn,
        &bookmark.name,
        &bookmark.url,
        bookmark.icon.as_deref(),
        &bookmark.category,
    )?;
    Ok((StatusCode::CREATED, Json(created)))
}

async fn update_bookmark(
    State(state): State<AppState>,
    _user: AuthUser,
    Path(id): Path<i32>,
    Json(input): Json<BookmarkInput>,
) -> Result<Json<Bookmark>, AppError> {
    let bookmark = clean_bookmark(input)?;
    let mut conn = db_conn(&state)?;
    let updated = db::update_bookmark(
        &mut conn,
        id,
        &bookmark.name,
        &bookmark.url,
        bookmark.icon.as_deref(),
        &bookmark.category,
    )?;
    Ok(Json(updated))
}

async fn delete_bookmark(
    State(state): State<AppState>,
    _user: AuthUser,
    Path(id): Path<i32>,
) -> Result<StatusCode, AppError> {
    let mut conn = db_conn(&state)?;
    db::delete_bookmark(&mut conn, id)?;
    Ok(StatusCode::NO_CONTENT)
}
