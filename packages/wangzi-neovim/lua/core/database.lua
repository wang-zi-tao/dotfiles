if vim.fn.has("win32") == 1 then
    vim.g.sqlite_clib_path = "C:/dotfiles-install/lib/sqlite3.dll"
end

local sqlite = require("sqlite.db")
local tbl = require("sqlite.tbl")

local M = {
    sqlite = sqlite,
    tbl = tbl,
    tables = {
        uri = vim.fn.stdpath("data") .. "/nvim.db",
        projects = tbl("projects", {
            path = { "text", unique = true, primary = true },
        }),
        caches = tbl("caches", {
            project = { "text" },
            key = { "text" },
            value = { "text" },
        }),
        breakpoints = tbl("breakpoints", {
            file = { "text" },
            line = { "integer" },
            project = { "text" },
        }),
        bookmarks = tbl("bookmarks", {
            project = { "text" },
            name = { "text" },
        }),
    },
}

local db = sqlite(M.tables)
M.db = db

function M.project_dir()
    return vim.fn.getcwd()
end

function M.getCachedValue(key)
    return M.tables.caches:get({
        where = {
            project = vim.fn.getcwd(),
            key = key,
        }
    })
end

function M.on_change_dir()
    if #M.tables.projects:get({ path = vim.fn.getcwd() }) == 0 then
        M.tables.caches:insert({
            project = M.project_dir(),
        })
    end
end

M.on_change_dir()

return M
