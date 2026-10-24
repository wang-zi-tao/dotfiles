//! Diesel 表结构定义，对应 migrations/ 下的迁移脚本。
//!
//! schema 由手写维护（也可用 diesel print-schema 生成），
//! 因此构建时不需要连接数据库。

diesel::table! {
    /// 收藏分类，position 决定展示顺序。
    categories (name) {
        name -> Text,
        position -> Integer,
    }
}

diesel::table! {
    /// 收藏条目。
    bookmarks (id) {
        id -> Integer,
        name -> Text,
        url -> Text,
        icon -> Nullable<Text>,
        category -> Text,
        position -> Integer,
        created_at -> BigInt,
        updated_at -> BigInt,
    }
}

diesel::table! {
    /// 简单的键值元数据表。
    meta (key) {
        key -> Text,
        value -> Text,
    }
}

diesel::table! {
    /// 登录会话，token 为随机生成的 64 位十六进制字符串。
    sessions (token) {
        token -> Text,
        username -> Text,
        created_at -> BigInt,
        expires_at -> BigInt,
    }
}

diesel::joinable!(bookmarks -> categories (category));

diesel::allow_tables_to_appear_in_same_query!(bookmarks, categories, meta, sessions);
