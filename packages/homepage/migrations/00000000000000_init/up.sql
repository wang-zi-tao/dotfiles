CREATE TABLE meta (
    key   TEXT PRIMARY KEY NOT NULL,
    value TEXT NOT NULL
);

CREATE TABLE categories (
    name     TEXT PRIMARY KEY NOT NULL,
    position INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE bookmarks (
    id         INTEGER PRIMARY KEY AUTOINCREMENT,
    name       TEXT NOT NULL,
    url        TEXT NOT NULL,
    icon       TEXT,
    category   TEXT NOT NULL DEFAULT '未分类',
    position   INTEGER NOT NULL DEFAULT 0,
    created_at BIGINT NOT NULL,
    updated_at BIGINT NOT NULL
);

CREATE INDEX idx_bookmarks_category ON bookmarks(category);

CREATE TABLE sessions (
    token      TEXT PRIMARY KEY NOT NULL,
    username   TEXT NOT NULL,
    created_at BIGINT NOT NULL,
    expires_at BIGINT NOT NULL
);
