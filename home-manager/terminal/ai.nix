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
  config = {
    home.packages = with pkgs; [
      mcp-nixos
      unstable.mcp-language-server
      uv
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
        context7 = {
          url = "https://mcp.context7.com/mcp";
          headers = {
            "CONTEXT7_API_KEY" = "YOUR_API_KEY";
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
      # package = pkgs.opencode-bunx;
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
        permission = {
          websearch = "allow";
          lsp = "allow";
        };
        plugin = [
          "oh-my-opencode-slim@latest"
          "oh-my-openagent@latest"
          "opencode-mem@latest"
          "@simonwjackson/opencode-direnv"
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

    home.file.".config/opencode/opencode-mem.json".text = builtins.toJSON {
      "embeddingModel" = "Xenova/nomic-embed-text-v1";
      "webServerEnabled" = true;
      "webServerPort" = 4747;
      "showAutoCaptureToasts" = true;
      "showUserProfileToasts" = true;
      "showErrorToasts" = true;
      "userProfileAnalysisInterval" = 10;
      "maxMemories" = 10;

      "compaction" = {
        "enabled" = true;
        "memoryLimit" = 10;
      };
      "chatMessage" = {
        "enabled" = true;
        "maxMemories" = 3;
        "excludeCurrentSession" = true;
        "injectOn" = "first";
      };

      "memoryProvider" = "openai-chat";
      "memoryModel" = "minimax-m2.5-free";
      "memoryApiUrl" = "https://opencode.ai/zen/v1/chat/completions";
      "memoryApiKey" = "file:///run/secrets/apikey/zen";
    };

    home.file.".config/opencode/oh-my-openagent.json".text =

      builtins.toJSON {
        "$schema" =
          "https://raw.githubusercontent.com/code-yeongyu/oh-my-openagent/dev/assets/oh-my-opencode.schema.json";
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
            model = deepseek-flash;
          };
          metis = {
            model = deepseek;
          };
          momus = {
            model = gpt5mini;
          };
          atlas = {
            model = gpt5mini;
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
            model = gpt5mini;
          };
          unspecified-high = {
            model = deepseek;
          };
          writing = {
            model = deepseek-flash;
          };
        };
      };

    home.sessionVariables = {
      OPENCODE_ENABLE_EXA = "true";
      OPENCODE_EXPERIMENTAL_LSP_TOOL = "true";
    };
  };
}
