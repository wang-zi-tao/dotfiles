{
  pkgs,
  config,
  lib,
  ...
}:
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
          command = "npx";
          args = [
            "-y"
            "mcp-neovim-server"
          ];
          env = {
            "ALLOW_SHELL_COMMANDS" = "true";
            "NVIM_SOCKET_PATH" = "/tmp/nvim";
          };
        };
        web-search = {
          command = "npx";
          args = [
            "open-websearch@latest"
          ];
          env = {
            "MODE" = "stdio";
            "DEFAULT_SEARCH_ENGINE" = "duckduckgo";
            "ALLOWED_SEARCH_ENGINES" = "duckduckgo;bing;exa";
          };
        };
        playwright = {
          command = "npx";
          args = [
            "-y"
            "@playwright/mcp@latest"
          ];
        };
        git = {
          command = "uvx";
          args = [
            "mcp-server-git"
          ];
        };
        obsidian = {
          command = "npx";
          args = [
            "-y"
            "mcp-obsidian"
            "${config.home.homeDirectory}/NextCloud/obsidian/"
          ];
        };
      };
    };
    xdg.configFile."mcphub/servers.json".text = builtins.toJSON {
      mcpServers = config.programs.mcp.servers;
    };

    programs.opencode = {
      enable = true;
      enableMcpIntegration = true;
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
        model = "deepseek/deepseek-chat";
        autoshare = false;
        autoupdate = false;
        provider = {
          deepseek = {
            options = {
              apiKey = "{file:/run/secrets/apikey/deepseek}";
            };
          };
        };
        permission = {
          websearch = "allow";
          lsp = "allow";
        };
      };
    };
    home.sessionVariables = {
      OPENCODE_EXPERIMENTAL = "true";
      OPENCODE_ENABLE_EXA = "true";
      OPENCODE_EXPERIMENTAL_LSP_TOOL = "true";
    };
  };
}
