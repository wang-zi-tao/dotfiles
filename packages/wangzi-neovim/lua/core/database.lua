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
            path = {"text", unique = true, primary = true},
            cwd = {"text"},
        } ),
        breakpoints = tbl("breakpoints", {
            file = {"text"},
            line = {"integer"},
            project = {"text"},
        } ),
        bookmarks = tbl("bookmarks", {
            project = {"text"},
            name = {"text"}
        } )
    },
}

local db = sqlite(M.tables) 
M.db = db

return M
