sqlite = require("sqlite")

local M = {}
M.sqlite = sqlite

local db = sqlite {
    uri = vim.fn.stdpath("data") .. "/nvim.db",
    projects = {
        path = {"text", unique = true, primary = true},
        cwd = {"text"},
    },
    breakpoints = {
        file = {"text"},
        line = {"integer"},
        project = {"text"},
    },
}
M.db = db

return M
