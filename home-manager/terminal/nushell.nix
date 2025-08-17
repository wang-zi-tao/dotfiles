{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = {
    programs.nushell =
      let
        scripts = pkgs.fetchgit {
          url = "https://github.com/nushell/nu_scripts";
          rev = "32cdc96414995e41de2a653719b7ae7375352eef";
          sha256 = "sha256-vn/YosQZ4OkWQqG4etNwISjzGJfxMucgC3wMpMdUwUg=";
        };
      in
      {
        enable = true;
        package = pkgs.unstable.nushell;
        shellAliases = {
          grep = "rg --color=auto";
          xclip = "xclip -selection c";
          s = "sudo su";
          j = "joshuto";

          gclone = "git clone";

          powertop = "sudo powertop";
          iotop = "sudo iotop";
          iftop = "sudo iftop";
          # nix-gc = "sudo nix-collect-garbage -d";
          top = "htop";
          htop = "htop";
          ps = "procs";

          # rm = "rmtrash -I";
          # mv = "rsync -avP --delete-delay";
          # mv-origin = "mv";
          b = "bat";
          cat = "bat";
          less = "bat --theme=Coldark-Dark";
          man = ''MANPAGER="sh -c 'col -bx | bat --theme=Coldark-Dark -l man -p'" man'';

          l = "eza -la --icons always";

          rts = "rg -C 8 -g '*.{ts}'";
          rkuip = "rg -C 8 -g '*.{kuip,ku}'";

          du = "dust";
          df = "duf";

          mux = "tmuxinator";
          tt = "tmux split -p 10";
          tsh = "tmux split -h";
          tsv = "tmux split -v";

          sudo = "sudo ";
          watch = "watch ";

          nlocate = "nix-locate --top-level";

          ".." = "cd ..";
        };

        configFile.text = with pkgs.unstable; ''
          use ${scripts}/custom-completions/git/git-completions.nu *
          use ${scripts}/custom-completions/make/make-completions.nu *
          use ${scripts}/custom-completions/cargo/cargo-completions.nu *
          use ${scripts}/custom-completions/nix/nix-completions.nu *
          use ${scripts}/custom-completions/zellij/zellij-completions.nu *
          use ${scripts}/custom-completions/virsh/virsh-completions.nu *
          use ${scripts}/custom-completions/zoxide/zoxide-completions.nu *
          use ${scripts}/custom-completions/ssh/ssh-completions.nu *
          use ${scripts}/custom-completions/tar/tar-completions.nu *
          use ${scripts}/custom-completions/rg/rg-completions.nu *
          use ${scripts}/custom-completions/curl/curl-completions.nu *
          use ${scripts}/custom-completions/docker/docker-completions.nu *
          use ${scripts}/custom-completions/bat/bat-completions.nu *

          use ${scripts}/aliases/git/git-aliases.nu *

          source ${scripts}/themes/nu-themes/tokyo-moon.nu

          plugin add ${nushellPlugins.units}/bin/nu_plugin_units
          plugin add ${nushellPlugins.polars}/bin/nu_plugin_polars
          plugin add ${nushellPlugins.query}/bin/nu_plugin_query
          plugin add ${nushellPlugins.net}/bin/nu_plugin_net
          plugin add ${nushellPlugins.highlight}/bin/nu_plugin_highlight
          plugin add ${nushellPlugins.gstat}/bin/nu_plugin_gstat
          plugin add ${nushellPlugins.formats}/bin/nu_plugin_formats

          $env.config.show_banner = false
        '';
      };
    programs.direnv = {
      enable = true;
      enableNushellIntegration = true;
    };
    programs.atuin = {
      enable = true;
      enableNushellIntegration = true;
    };
    programs.carapace = {
      enable = true;
      enableNushellIntegration = true;
    };
    programs.yazi = {
      enable = true;
      enableNushellIntegration = true;
    };
    programs.keychain.enableNushellIntegration = true;
    programs.zoxide = {
      enable = true;
      enableNushellIntegration = true;
    };
    programs.starship = {
      enable = true;
      enableNushellIntegration = true;
      settings = {
        add_newline = true;
        scan_timeout = 10;
        format = ''
          [](254)[$os](bg:254 blue)[  $directory](254 bg:blue)[](bg:11 fg:blue)[  $git_branch$git_commit$git_state$git_metrics$git_status](bg:11 black)[](fg:11) $all [$character](blue)
        '';
        directory = {
          style = "bg:blue 254";
        };
        git_state = {
          format = "[ $state ($progress_current of $progress_total) ] ($style) ";
          style = "black bg:11";
          cherry_pick = "[🍒 PICKING](bold red)";
        };
        git_branch = {
          format = "[$symbol$branch(:$remote_branch)]($style) ";
          style = "bg:11 black";
        };
        git_status = {
          format = "([\\[$all_status$ahead_behind\\]]($style) )";
          style = "bg:11 black";
          ahead = "⇡\${count} ";
          diverged = "⇕⇡\${ahead_count}⇣\${behind_count} ";
          behind = "⇣\${count} ";
          modified = "!\${count} ";
          stashed = "s\${count} ";
          staged = "+\${count} ";
          untracked = "?\${count} ";
          conflicted = "=\${count} ";
        };
        git_metrics = {
          added_style = "bg:11 black";
          deleted_style = "bg:11 black";
          format = ''[+$added]($added_style)/[-$deleted]($deleted_style) '';
          disabled = false;
        };
        nix_shell = { };
        os = {
          style = "bg:254 fg:blue";
          disabled = false;
        };
      };
    };
  };
}
