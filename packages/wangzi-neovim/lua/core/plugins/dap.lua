local dap = require("dap")
local n = require("core.gen")
dap.adapters.lldb = {
  type = "executable",
  command = "lldb-vscode", -- adjust as needed
  name = "lldb",
}
local path_cache = vim.fn.getcwd() .. "/"
local program = function()
  path_cache = vim.fn.input("Path to executable: ", path_cache, "file")
  return path_cache
end
dap.adapters.cppdbg = {
  id = 'cppdbg',
  type = 'executable',
  command = (n.cpptools or '') .. '/share/vscode/extensions/ms-vscode.cpptools/debugAdapters/bin/OpenDebugAD7',
}
dap.configurations.cpp = {
  {
    name = "Launch lldb",
    type = "lldb",
    request = "launch",
    program = program,
    cwd = "${workspaceFolder}",
    -- stopOnEntry = true,
  },
  {
    name = "Launch gdb",
    type = "cppdbg",
    request = "launch",
    program = program,
    cwd = '${workspaceFolder}',
    stopOnEntry = true,
  },
  {
    name = 'Attach to gdbserver :1234',
    type = 'cppdbg',
    request = 'launch',
    MIMode = 'gdb',
    miDebuggerServerAddress = 'localhost:1234',
    miDebuggerPath = 'gdb',
    cwd = '${workspaceFolder}',
    program = program,
  },
}

dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp

dap.adapters.markdown = {
  type = "executable",
  name = "mockdebug",
  command = "node",
  args = { "./out/debugAdapter.js" },
  cwd = "path/to/vscode-mock-debug/"
}

dap.configurations.markdown = {
  {
    type = "mock",
    request = "launch",
    name = "mock test",
    program = "/path/to/a/readme.md",
    stopOnEntry = true,
    debugServer = 4711
  }
}

-- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
dap.adapters.go = {
  type = 'executable';
  command = 'node';
  args = { os.getenv('HOME') .. '/dev/golang/vscode-go/dist/debugAdapter.js' };
}
dap.configurations.go = {
  {
    type = 'go';
    name = 'Debug';
    request = 'launch';
    showLog = false;
    program = "${file}";
    dlvToolPath = vim.fn.exepath('dlv') -- Adjust to where delve is installed
  },
}

dap.adapters.python = {
  type = 'executable';
  command = 'python';
  args = { '-m', 'debugpy.adapter' };
}
dap.configurations.python = {
  {
    -- The first three options are required by nvim-dap
    type = 'python'; -- the type here established the link to the adapter definition: `dap.adapters.python`
    request = 'launch';
    name = "Launch file";

    -- Options below are for debugpy, see https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings for supported options

    program = "${file}"; -- This configuration will launch the current file if used.
    pythonPath = function()
      -- debugpy supports launching an application with a different interpreter then the one used to launch debugpy itself.
      -- The code below looks for a `venv` or `.venv` folder in the current directly and uses the python within.
      -- You could adapt this - to for example use the `VIRTUAL_ENV` environment variable.
      local cwd = vim.fn.getcwd()
      if vim.fn.executable(cwd .. '/venv/bin/python') == 1 then
        return cwd .. '/venv/bin/python'
      elseif vim.fn.executable(cwd .. '/.venv/bin/python') == 1 then
        return cwd .. '/.venv/bin/python'
      else
        return '/usr/bin/python'
      end
    end;
  },
}
