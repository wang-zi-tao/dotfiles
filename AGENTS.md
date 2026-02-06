# AGENTS.md - NixOS Dotfiles Configuration Guide

This document provides guidelines for AI agents working with this NixOS dotfiles repository.

## Project Overview

This is a personal NixOS configuration repository for managing multiple systems (PCs, servers, VMs, Android devices) with distributed system components including SeaweedFS, WireGuard, k3s, LXD, and CRIU.

## Build/Lint/Test Commands

### Nix Development
```bash
# Enter development shell with all tools
nix develop

# Build specific package
nix build .#<package-name>

# Build all packages
nix build .#packages

# Format Nix files with nixfmt
nixfmt-rfc-style *.nix

# Check Nix code with statix
statix check

# Fix Nix code with statix
statix fix
```

### Testing
```bash
# No specific test framework - uses Neovim's neotest plugin
# Run tests within Neovim using neotest commands
# Build and test packages with Nix
nix build .#<package-name> && ./result/bin/<executable> --test
```

### System Management
```bash
# Build specific machine configuration
nix build .#nixosConfigurations.<machine-name>.config.system.build.toplevel

# Build home-manager configuration
nix build .#homeConfigurations.<user>@<host>.activationPackage

# Deploy to remote nodes
nix run .#apps.deploy-rs -- <node-name>

# Generate ISO image
nix build .#nixosConfigurations.<machine-name>.config.system.build.isoImage
```

### Lua Formatting
```bash
# Format Lua files (uses stylua config from stylua.toml)
stylua --config-path stylua.toml <file.lua>

# Format Neovim Lua configuration files
stylua --config-path stylua.toml packages/wangzi-neovim/lua/**/*.lua
```

## Code Style Guidelines

### Nix Language
- **Imports**: Use relative imports for local modules, absolute for external
- **Formatting**: Use `nixfmt-rfc-style` with 2-space indentation
- **Naming**: snake_case for variables and functions, camelCase for attributes
- **Types**: Use type annotations in function arguments when helpful
- **Error Handling**: Use `assert` for preconditions, `throw` for unrecoverable errors
- **Structure**: Organize flake outputs in logical sections (nixosConfigurations, homeConfigurations, packages, etc.)

Example Nix style:
```nix
{ lib, pkgs, config, ... }:

let
  myPackage = pkgs.callPackage ./my-package.nix { };
in {
  options = {
    myModule.enable = lib.mkEnableOption "my module";
  };

  config = lib.mkIf config.myModule.enable {
    environment.systemPackages = [ myPackage ];
  };
}
```

### Lua Language (Neovim & AwesomeWM configuration)
- **Imports**: Use `require()` for modules, group related requires at top of file
- **Formatting**: 4-space indentation, 120 column width (per stylua.toml)
- **Naming**: snake_case for variables and functions, PascalCase for modules
- **Error Handling**: Use `pcall()` for protected calls, handle errors gracefully
- **Structure**: Modular organization with separate files for plugins, keymaps, options

Example Neovim Lua style:
```lua
local utils = require("core.utils")
local keymap = vim.keymap.set

local M = {}

function M.setup()
    -- Use 4-space indentation
    keymap("n", "<leader>ff", function()
        require("telescope.builtin").find_files()
    end, { desc = "Find files" })
end

return M
```

Example AwesomeWM Lua style:
```lua
local beautiful = require("beautiful")
local awful = require("awful")

local function setup_keys()
    awful.keyboard.append_global_keybindings({
        awful.key({ modkey }, "Return", function()
            awful.spawn(terminal)
        end, { description = "open terminal", group = "launcher" }),
    })
end

return {
    setup_keys = setup_keys,
}
```

### Shell Scripts
- **Shebang**: Always use `#!/usr/bin/env bash`
- **Formatting**: 2-space indentation, use shellcheck for linting
- **Error Handling**: Use `set -euo pipefail` at script start
- **Variables**: Use lowercase with underscores for local variables, UPPERCASE for exported

### Python (Ranger plugins)
- **Formatting**: Follow PEP 8, 4-space indentation
- **Imports**: Group standard library, third-party, local imports
- **Naming**: snake_case for functions/variables, PascalCase for classes

## File Organization Conventions

### Directory Structure
```
├── machine/          # NixOS machine configurations
├── home-manager/     # User configuration modules
│   ├── application/  # Application configs
│   ├── desktop/      # Desktop environment configs
│   └── develop/      # Development tools configs
├── packages/         # Custom Nix packages
├── overlays/         # Nixpkgs overlays
├── module/           # Reusable NixOS modules
├── nix-on-droid/     # Android device configs
├── darwin/           # macOS configs
├── scripts/          # Utility scripts
└── secrets/          # Encrypted secrets (SOPS)
```

### Naming Patterns
- Machine configs: `machine/<hostname>/machine.nix`
- Home configs: `home-manager/<category>/<name>.nix`
- Packages: `packages/<name>/default.nix`
- Modules: `module/<name>/module.nix`

## Security Guidelines

### Secrets Management
- **SOPS**: All secrets are encrypted with SOPS using age keys
- **Location**: Store encrypted secrets in `secrets/` directory
- **Never commit**: Unencrypted secrets or keys
- **Access**: Reference secrets via `sops.secrets` in Nix configurations

### Safe Practices
- Use `allowUnfree = true` only in specific contexts
- Validate external inputs in scripts
- Use least privilege for service accounts
- Encrypt sensitive data in transit and at rest

## Development Workflow

### Adding New Packages
1. Create `packages/<name>/default.nix`
2. Add to appropriate system or user packages list
3. Test with `nix build .#<package-name>`

### Adding Machine Configuration
1. Create `machine/<hostname>/machine.nix`
2. Import necessary modules
3. Test build with `nix build .#nixosConfigurations.<hostname>`

### Modifying Home Configuration
1. Edit relevant file in `home-manager/`
2. Test with `home-manager switch` or build activation package

### Testing Changes
```bash
# Dry-run deployment
nix run .#apps.deploy-rs -- --dry-run <node>

# Build without installing
nix build .#nixosConfigurations.<host>.config.system.build.toplevel

# Check syntax
nix-instantiate --eval --strict <file.nix>
```

## Common Patterns

### Flake Output Structure
```nix
{
  nixosConfigurations = { /* machine configs */ };
  homeConfigurations = { /* user configs */ };
  packages = { /* custom packages */ };
  devShells.default = /* development environment */;
  apps.deploy-rs = /* deployment tool */;
}
```

### Module Pattern
```nix
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.myService;
in {
  options.services.myService = {
    enable = mkEnableOption "My Service";
    port = mkOption {
      type = types.port;
      default = 8080;
    };
  };

  config = mkIf cfg.enable {
    systemd.services.myService = {
      /* service definition */
    };
  };
}
```

## Tool Configuration

### Editor Configurations
- **Neovim**: Custom configuration in `packages/wangzi-neovim/`
- **VSCode**: Configuration in `home-manager/develop/vscode.nix`
- **StyLua**: Configuration in `stylua.toml`
- **Lua LS**: Configuration in `.luarc.json`

### Neovim Package Structure
The `packages/wangzi-neovim/` directory contains a custom Neovim configuration:
- `lua/core/`: Core Neovim configuration modules
- `lua/core/plugins/`: Plugin configurations organized by category
- `skeleton/`: Project templates and development environment files
- `snippets/`: Code snippets for various languages
- `init.lua`: Main entry point for Neovim configuration

Key modules:
- `core/init.lua`: Loads all core modules
- `core/opt.lua`: Neovim options and settings
- `core/map.lua`: Key mappings and which-key configurations
- `core/utils.lua`: Utility functions and helpers
- `core/plugins/`: Plugin configurations (LSP, UI, navigation, etc.)

### Git Configuration
- Configured in `home-manager/develop/git.nix`
- Uses neovim-remote for difftool/mergetool
- Custom aliases for common operations

### Package Development
- **Custom Packages**: Defined in `packages/<name>/default.nix`
- **Overlays**: Custom package modifications in `overlays/`
- **Nix Functions**: Use `callPackage` for package definitions
- **Dependencies**: Declare all inputs in flake.nix

## Performance Considerations

- Use `callPackage` for package definitions
- Leverage Nix's caching mechanisms
- Minimize rebuilds with targeted changes
- Use `import <nixpkgs> {}` only when necessary

## Troubleshooting

### Common Issues
1. **Build failures**: Check `nix log` for detailed error messages
2. **Missing dependencies**: Ensure all inputs are declared in flake.nix
3. **Format issues**: Run `nixfmt-rfc-style` and `stylua` as needed
4. **Secret decryption**: Ensure proper SOPS keys are available

### Debug Commands
```bash
# Show dependency graph
nix-store --query --references $(nix-build .#<package>)

# Check closure size
nix path-info -S $(nix-build .#<package>)

# Profile builds
nix build .#<package> --profile /tmp/profile
```

This repository represents a sophisticated NixOS configuration system. Always respect the existing patterns and security practices when making changes.