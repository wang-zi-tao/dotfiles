local function config()
    local ollama_host = vim.env.OLLAMA_HOST
    local host = vim.env.HOST
    if host == "wangzi-nuc" or host == "wangzi-asus" then
        ollama_host = "wangzi-pc.wg"
    end
    ollama_host = ollama_host or "localhost"
    local gen = require("gen")
    gen.prompts["Elaborate_Text"] = {
        prompt = "Elaborate the following text:\n$text",
        replace = true,
    }
    gen.prompts["Fix_Code"] = {
        prompt =
        "Fix the following code. Only ouput the result in format ```$filetype\n...\n```:\n```$filetype\n$text\n```",
        replace = true,
        extract = "```$filetype\n(.-)```",
    }
    gen.setup({
        model = vim.env.OLLAMA_MODEL or "deepseek-coder:6.7b", -- The default model to use.
        host = ollama_host,                                    -- The host running the Ollama service.
        port = vim.env.OLLAMA_PORT or "11434",                 -- The port on which the Ollama service is listening.
        quit_map = "q",                                        -- set keymap for close the response window
        retry_map = "<c-r>",                                   -- set keymap to re-send the current prompt
        init = function(options)
            if vim.fn.has("win32") == 1 or vim.env.OLLAMA_LAUNCH then
                pcall(io.popen, "ollama serve")
            end
        end,
        -- Function to initialize Ollama
        command = function(options)
            local body = { model = options.model, stream = true }
            return "curl --noproxy '*' --silent --no-buffer -X POST http://"
                .. options.host
                .. ":"
                .. options.port
                .. "/api/chat -d $body"
        end,
        -- The command for the Ollama service. You can use placeholders $prompt, $model and $body (shellescaped).
        -- This can also be a command string.
        -- The executed command must return a JSON object with { response, context }
        -- (context property is optional).
        -- list_models = '<omitted lua function>', -- Retrieves a list of model names
        display_mode = "horizontal-split", -- The display mode. Can be "float" or "split" or "horizontal-split".
        show_prompt = true,                -- Shows the prompt submitted to Ollama.
        show_model = true,                 -- Displays which model you are using at the beginning of your chat session.
        no_auto_close = false,             -- Never closes the window automatically.
        debug = false,                     -- Prints errors and the command which is run.
    })
end
return {
    {
        "David-Kunz/gen.nvim",
        dir = gen.gen_nvim,
        name = "codecompanion",
        cmd = { "Gen" },
        config = config,
        init = function()
            require("which-key").add({
                { "<leader>a", group = "AI ollama" },
            })
        end,
        keys = {
            { "<leader>a",  [[<cmd>Gen<CR>]],      mode = { "n", "v" }, desc = "AI" },
            { "<leader>ac", [[<cmd>Gen Chat<CR>]], desc = "AI" },
        },
    },
}
