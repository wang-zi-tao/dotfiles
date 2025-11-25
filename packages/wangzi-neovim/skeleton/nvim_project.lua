local dap = require("dap")
local overseer = require("overseer")
local util = require("core.utils")

util.override_dap_config("lldb launch", "cpp", {
    name = "lldb launch with arg",
    program = util.get_program,
    programArgs = {},
})

util.override_dap_config("vsdbg launch", "cpp", {
	name = "vsdbg launch with arg",
	request = "launch",
    program = util.get_program,
	args = {},
})

overseer.register_template({
	name = "build-wps",
	description = "Build WPS Office applications",
	priority = 100,
	builder = function()
		return {
			cmd = { "krepo-ng", "build", "--with-3rd" },
			cwd = "../debug"
		}
	end,
})
