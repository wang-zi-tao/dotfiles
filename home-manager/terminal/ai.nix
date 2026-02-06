{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = {
    programs.mcp = {
      enable = true;
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
      };
    };
  };
}
