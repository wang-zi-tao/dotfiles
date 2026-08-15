{
  pkgs,
  config,
  lib,
  ...
}:
let
  kimi-k26 = "moonshotai-cn/kimi-k2.6";
  kimi = "moonshotai-cn/kimi-k2.5";
  deepseek = "deepseek/deepseek-v4-pro";
  deepseek-flash = "deepseek/deepseek-v4-flash";
  gpt5mini = "github-copilot/gpt-5-mini";
  minimax = "opencode/minimax-m2.5-free";
in
{
  imports = [
    ./dsh/dsh.nix
  ];
  config = {
    home.packages = with pkgs; [
      mcp-nixos
      unstable.mcp-language-server
      uv

      pnpm
      nodejs
    ];
    programs.mcp = {
      enable = true;
      servers = {
        nixos = {
          command = "mcp-nixos";
        };
        neovim = {
          command = "${pkgs.mcp-neovim-server}/bin/mcp-neovim-server";
          env = {
            "ALLOW_SHELL_COMMANDS" = "true";
            "NVIM_SOCKET_PATH" = "/tmp/nvim";
          };
        };
      };
    };
    xdg.configFile."mcphub/servers.json".text = builtins.toJSON {
      mcpServers = config.programs.mcp.servers;
    };

    programs.opencode = {
      enable = true;
      enableMcpIntegration = true;
      # https://github.com/NixOS/nixpkgs/blob/master/pkgs/by-name/op/opencode/package.nix
      package = pkgs.llm-agents.opencode;
      agents = {
        code-reviewer = ''
          # Code Reviewer Agent

          You are a senior software engineer specializing in code reviews.
          Focus on code quality, security, and maintainability.

          ## Guidelines
          - Review for potential bugs and edge cases
          - Check for security vulnerabilities
          - Ensure code follows best practices
          - Suggest improvements for readability and performance
        '';
      };
      settings = {
        theme = "tokyonight";
        model = "deepseek/deepseek-v4-pro";
        autoshare = false;
        autoupdate = false;
        provider = {
          deepseek = {
            options = {
              apiKey = "{file:/run/secrets/apikey/deepseek}";
            };
          };
          moonshotai-cn = {
            options = {
              apiKey = "{file:/run/secrets/apikey/moonshotai-cn}";
            };
          };
          zen = {
            options = {
              apiKey = "{file:/run/secrets/apikey/zen}";
            };
          };
        };
        mcp = {
          context7 = {
            type = "remote";
            url = "https://mcp.context7.com/mcp";
            enabled = true;
            headers = {
              Authorization = "Bearer {file:/run/secrets/apikey/context7}";
            };
          };
        };
        permission = {
          websearch = "allow";
          lsp = "allow";
        };
        plugin = [
          "oh-my-openagent@latest"
          # pkgs.llm-agents.oh-my-opencode
          "@simonwjackson/opencode-direnv"
          "@tarquinen/opencode-dcp@latest"
          "@cortexkit/aft-opencode@latest"
          [
            "@vectorize-io/opencode-hindsight"
            {
              hindsightApiUrl = "http://127.0.0.1:11438";
              hindsightApiToken = "{file:/run/secrets/hindsight/apikey}";
            }
          ]
        ];
      };
    };

    home.file.".config/opencode/tui.json".text = builtins.toJSON {
      "theme" = "tokyonight";
    };

    home.file.".config/opencode/oh-my-opencode-slim.json".text = builtins.toJSON {
      "$schema" =
        "https://raw.githubusercontent.com/code-yeongyu/oh-my-opencode/dev/assets/oh-my-opencode-slim.schema.json";
      agents = {
      };
    };

    home.file.".config/opencode/oh-my-openagent.json".text =

      builtins.toJSON {
        "$schema" =
          "https://raw.githubusercontent.com/code-yeongyu/oh-my-openagent/dev/assets/oh-my-opencode.schema.json";
        git_master = {
          commit_footer = true;
          include_co_authored_by = true;
          git_env_prefix = "GIT_MASTER=1";
        };
        team_mode = {
          enabled = true;
          max_parallel_members = 4;
          tmux_visualization = false;
        };
        codegraph = {
          daemon = true;
        };
        sisyphus_agent = {
          default_builder_enabled = true;
        };
        background_task = {
          defaultConcurrency = 16;
        };
        experimental = {
          task_system = true;
          aggressive_truncation = true;
        };
        hashline_edit = true;
        disabled_tools = [
          "lsp_goto_definition"
          "lsp_find_references"
          "lsp_symbols"
          "lsp_diagnostics"
          "lsp_prepare_rename"
          "lsp_rename"
        ];
        disabled_mcps = [
          "context7"
        ];
        agents = {
          sisyphus = {
            model = deepseek;
          };
          hephaestus = {
            model = kimi-k26;
          };
          oracle = {
            model = kimi-k26;
            variant = "high";
          };
          explore = {
            model = deepseek-flash;
          };
          librarian = {
            model = deepseek-flash;
          };
          multimodal-looker = {
            model = kimi;
          };
          prometheus = {
            model = deepseek;
          };
          metis = {
            model = deepseek;
          };
          momus = {
            model = deepseek-flash;
          };
          atlas = {
            model = deepseek-flash;
          };
          sisyphus-junior = {
            model = deepseek-flash;
          };
        };
        categories = {
          visual-engineering = {
            model = kimi;
          };
          ultrabrain = {
            model = kimi-k26;
          };
          artistry = {
            model = deepseek-flash;
          };
          quick = {
            model = minimax;
          };
          unspecified-low = {
            model = deepseek-flash;
          };
          unspecified-high = {
            model = deepseek;
          };
          writing = {
            model = deepseek;
          };
        };
      };

    home.sessionVariables = {
      HERMES_HOME = "/var/lib/hermes/.hermes";
      HERMES_TUI = "1";
      HERMES_TUI_RESUME = "1";
      OPENCODE_ENABLE_EXA = "true";
      OPENCODE_EXPERIMENTAL_LSP_TOOL = "true";
    };
  };
}
