use std::fs;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use diesel::connection::SimpleConnection;
use diesel::dsl::sql;
use diesel::prelude::*;
use diesel::r2d2::{ConnectionManager, CustomizeConnection};
use diesel::sql_types::Integer;
use diesel::sqlite::SqliteConnection;
use diesel_migrations::{embed_migrations, EmbeddedMigrations, MigrationHarness};
use serde::Serialize;

use crate::api::AppError;
use crate::schema::{bookmarks, categories, meta, sessions};

pub const DEFAULT_CATEGORY: &str = "未分类";
pub const MIGRATIONS: EmbeddedMigrations = embed_migrations!("migrations");

pub type DbPool = diesel::r2d2::Pool<ConnectionManager<SqliteConnection>>;
pub type DbConn = diesel::r2d2::PooledConnection<ConnectionManager<SqliteConnection>>;

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Bookmark {
    pub id: i32,
    pub name: String,
    pub url: String,
    pub icon: Option<String>,
    pub category: String,
}

#[derive(Debug, Queryable)]
struct BookmarkRow {
    id: i32,
    name: String,
    url: String,
    icon: Option<String>,
    category: String,
}

impl From<BookmarkRow> for Bookmark {
    fn from(row: BookmarkRow) -> Self {
        Self {
            id: row.id,
            name: row.name,
            url: row.url,
            icon: row.icon,
            category: row.category,
        }
    }
}

#[derive(Insertable)]
#[diesel(table_name = bookmarks)]
struct NewBookmark<'a> {
    name: &'a str,
    url: &'a str,
    icon: Option<&'a str>,
    category: &'a str,
    position: i32,
    created_at: i64,
    updated_at: i64,
}

#[derive(Insertable)]
#[diesel(table_name = categories)]
struct NewCategory<'a> {
    name: &'a str,
    position: i32,
}

#[derive(Insertable)]
#[diesel(table_name = sessions)]
struct NewSession<'a> {
    token: &'a str,
    username: &'a str,
    created_at: i64,
    expires_at: i64,
}

#[derive(Insertable)]
#[diesel(table_name = meta)]
struct NewMeta<'a> {
    key: &'a str,
    value: String,
}

type BookmarkColumns = (
    bookmarks::id,
    bookmarks::name,
    bookmarks::url,
    bookmarks::icon,
    bookmarks::category,
);

fn bookmark_columns() -> BookmarkColumns {
    (
        bookmarks::id,
        bookmarks::name,
        bookmarks::url,
        bookmarks::icon,
        bookmarks::category,
    )
}

pub fn now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs() as i64
}

#[derive(Debug)]
struct SqlitePragmas;

impl CustomizeConnection<SqliteConnection, diesel::r2d2::Error> for SqlitePragmas {
    fn on_acquire(&self, conn: &mut SqliteConnection) -> Result<(), diesel::r2d2::Error> {
        conn.batch_execute(
            "PRAGMA journal_mode=WAL; PRAGMA synchronous=NORMAL; PRAGMA busy_timeout=5000;",
        )
        .map_err(diesel::r2d2::Error::QueryError)
    }
}

/// 打开数据库连接池，并在启动时执行内嵌迁移。
pub fn open(path: &Path) -> Result<DbPool, AppError> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|err| {
            AppError::internal(format!("创建数据库目录失败 {}: {err}", parent.display()))
        })?;
    }

    let manager =
        ConnectionManager::<SqliteConnection>::new(path.to_string_lossy().into_owned());
    let pool = diesel::r2d2::Pool::builder()
        .max_size(8)
        .connection_customizer(Box::new(SqlitePragmas))
        .build(manager)
        .map_err(|err| AppError::internal(format!("初始化数据库连接池失败: {err}")))?;

    let mut conn = pool
        .get()
        .map_err(|err| AppError::internal(format!("获取数据库连接失败: {err}")))?;
    conn.run_pending_migrations(MIGRATIONS)
        .map_err(|err| AppError::internal(format!("执行数据库迁移失败: {err}")))?;
    Ok(pool)
}

pub fn bookmark_count(conn: &mut SqliteConnection) -> Result<i64, AppError> {
    bookmarks::table.count().get_result(conn).map_err(app_error)
}

/// 首次启动且数据库为空时，把源码目录里的简易收藏 JSON 导入为初始收藏。
pub fn seed_from_json(conn: &mut SqliteConnection, path: &Path) -> Result<usize, AppError> {
    if bookmark_count(conn)? > 0 {
        return Ok(0);
    }
    let document = crate::seed::SeedDocument::load(path)
        .map_err(|err| AppError::internal(format!("加载初始收藏失败: {err}")))?;
    if document.categories.is_empty() {
        return Ok(0);
    }

    let ts = now();
    conn.transaction::<usize, AppError, _>(|conn| {
        let mut total = 0usize;
        for (cat_index, category) in document.categories.iter().enumerate() {
            diesel::insert_into(categories::table)
                .values(NewCategory {
                    name: &category.name,
                    position: cat_index as i32,
                })
                .on_conflict(categories::name)
                .do_nothing()
                .execute(conn)?;
            for (index, bookmark) in category.bookmarks.iter().enumerate() {
                diesel::insert_into(bookmarks::table)
                    .values(NewBookmark {
                        name: &bookmark.name,
                        url: &bookmark.url,
                        icon: bookmark.icon.as_deref(),
                        category: &category.name,
                        position: index as i32,
                        created_at: ts,
                        updated_at: ts,
                    })
                    .execute(conn)?;
                total += 1;
            }
        }
        diesel::insert_into(meta::table)
            .values(NewMeta {
                key: "seeded_at",
                value: ts.to_string(),
            })
            .on_conflict(meta::key)
            .do_nothing()
            .execute(conn)?;
        Ok(total)
    })
}

pub fn list_bookmarks(conn: &mut SqliteConnection) -> Result<Vec<Bookmark>, AppError> {
    let rows = bookmarks::table
        .left_join(categories::table.on(categories::name.eq(bookmarks::category)))
        .select(bookmark_columns())
        .order_by(sql::<Integer>("COALESCE(categories.position, 999999)"))
        .then_order_by(bookmarks::position)
        .then_order_by(bookmarks::id)
        .load::<BookmarkRow>(conn)
        .map_err(app_error)?;
    Ok(rows.into_iter().map(Bookmark::from).collect())
}

fn ensure_category(conn: &mut SqliteConnection, name: &str) -> Result<(), AppError> {
    let exists = categories::table
        .find(name)
        .select(categories::name)
        .first::<String>(conn)
        .optional()
        .map_err(app_error)?
        .is_some();
    if !exists {
        let max_position = categories::table
            .select(diesel::dsl::max(categories::position))
            .first::<Option<i32>>(conn)
            .map_err(app_error)?;
        diesel::insert_into(categories::table)
            .values(NewCategory {
                name,
                position: max_position.unwrap_or(-1) + 1,
            })
            .execute(conn)?;
    }
    Ok(())
}

fn next_position(conn: &mut SqliteConnection, category: &str) -> Result<i32, AppError> {
    let max_position = bookmarks::table
        .filter(bookmarks::category.eq(category))
        .select(diesel::dsl::max(bookmarks::position))
        .first::<Option<i32>>(conn)
        .map_err(app_error)?;
    Ok(max_position.unwrap_or(-1) + 1)
}

#[allow(clippy::too_many_arguments)]
pub fn create_bookmark(
    conn: &mut SqliteConnection,
    name: &str,
    url: &str,
    icon: Option<&str>,
    category: &str,
) -> Result<Bookmark, AppError> {
    let ts = now();
    conn.transaction::<Bookmark, AppError, _>(|conn| {
        ensure_category(conn, category)?;
        let position = next_position(conn, category)?;
        let row = diesel::insert_into(bookmarks::table)
            .values(NewBookmark {
                name,
                url,
                icon,
                category,
                position,
                created_at: ts,
                updated_at: ts,
            })
            .returning(bookmark_columns())
            .get_result::<BookmarkRow>(conn)
            .map_err(app_error)?;
        Ok(row.into())
    })
}

#[allow(clippy::too_many_arguments)]
pub fn update_bookmark(
    conn: &mut SqliteConnection,
    id: i32,
    name: &str,
    url: &str,
    icon: Option<&str>,
    category: &str,
) -> Result<Bookmark, AppError> {
    conn.transaction::<Bookmark, AppError, _>(|conn| {
        let (existing_category, existing_position) = bookmarks::table
            .find(id)
            .select((bookmarks::category, bookmarks::position))
            .first::<(String, i32)>(conn)
            .optional()
            .map_err(app_error)?
            .ok_or_else(|| AppError::not_found("收藏不存在"))?;

        let position = if existing_category == category {
            existing_position
        } else {
            ensure_category(conn, category)?;
            next_position(conn, category)?
        };

        let row = diesel::update(bookmarks::table.find(id))
            .set((
                bookmarks::name.eq(name),
                bookmarks::url.eq(url),
                bookmarks::icon.eq(icon),
                bookmarks::category.eq(category),
                bookmarks::position.eq(position),
                bookmarks::updated_at.eq(now()),
            ))
            .returning(bookmark_columns())
            .get_result::<BookmarkRow>(conn)
            .map_err(app_error)?;
        Ok(row.into())
    })
}

pub fn delete_bookmark(conn: &mut SqliteConnection, id: i32) -> Result<(), AppError> {
    let deleted = diesel::delete(bookmarks::table.find(id))
        .execute(conn)
        .map_err(app_error)?;
    if deleted == 0 {
        return Err(AppError::not_found("收藏不存在"));
    }
    Ok(())
}

pub fn create_session(
    conn: &mut SqliteConnection,
    token: &str,
    username: &str,
    ttl_seconds: i64,
) -> Result<(), AppError> {
    let ts = now();
    diesel::insert_into(sessions::table)
        .values(NewSession {
            token,
            username,
            created_at: ts,
            expires_at: ts + ttl_seconds,
        })
        .execute(conn)
        .map_err(app_error)?;
    Ok(())
}

pub fn session_user(conn: &mut SqliteConnection, token: &str) -> Result<Option<String>, AppError> {
    sessions::table
        .filter(sessions::token.eq(token))
        .filter(sessions::expires_at.gt(now()))
        .select(sessions::username)
        .first::<String>(conn)
        .optional()
        .map_err(app_error)
}

pub fn delete_session(conn: &mut SqliteConnection, token: &str) -> Result<(), AppError> {
    diesel::delete(sessions::table.filter(sessions::token.eq(token)))
        .execute(conn)
        .map_err(app_error)?;
    Ok(())
}

pub fn purge_expired_sessions(conn: &mut SqliteConnection) -> Result<(), AppError> {
    diesel::delete(sessions::table.filter(sessions::expires_at.le(now())))
        .execute(conn)
        .map_err(app_error)?;
    Ok(())
}

fn app_error(err: diesel::result::Error) -> AppError {
    AppError::internal(format!("数据库错误: {err}"))
}

