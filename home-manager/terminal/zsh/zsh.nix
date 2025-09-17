{
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [ ];
  programs.zsh = {
    enable = true;
    autocd = true;
    autosuggestion.enable = true;
    oh-my-zsh = {
      enable = false;
      # theme = "agnoster";
      plugins = [
        "git"
        "extract"
        "colored-man-pages"
        "sudo"
        "command-not-found"
        "python"
        "vi-mode"
        # "autojump"
        "colorize"
        "tmux"
        "gitignore"
        "safe-paste"
      ];
    };
    prezto = {
      enable = true;
      python.virtualenvAutoSwitch = true;
      screen.autoStartLocal = true;
      syntaxHighlighting.highlighters = [
        "main"
        "brackets"
        "line"
      ];
      pmodules = [
        "autosuggestions"
        "environment"
        "git"
        "ssh"
        "tmux"
        "rsync"
        "archive"
        "directory"
        "editor"
        "history"
        "docker"
        "completion"
        "command-not-found"
        "syntax-highlighting"
        "history-substring-search"
      ];
      editor.keymap = "vi";
      editor.dotExpansion = true;
      historySubstring.foundColor = "fg=blue";
      historySubstring.notFoundColor = "dg=red";
      tmux = {
        autoStartRemote = false;
      };
    };
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.zsh-nix-shell;
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.zsh-syntax-highlighting;
      }
      {
        name = "zsh-history-substring-search";
        src = pkgs.zsh-history-substring-search;
      }
    ];
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
      cat = "bat";
      less = "bat --theme=Coldark-Dark";
      man = ''MANPAGER="sh -c 'col -bx | bat --theme=Coldark-Dark -l man -p'" man'';

      ls = "eza --icons always";
      ll = "eza -la --icons always";
      l = "eza -la --icons always";

      rcpp = "rg -C=8 -g='*.{c,cpp,h,hpp,mm,inl,INL}'";
      rts = "rg -C=8 -g='*.{ts}'";
      rkuip = "rg -C=8 -g='*.{kuip,ku}'";

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

      podmanexec = "podman exec -e DISPLAY=$DISPLAY -w $PWD -ti wps_debug";
    };
    sessionVariables = config.home.sessionVariables // {
      _ZO_EXCLUDE_DIRS = "/nix";
    };
    initContent =
      builtins.readFile ./p10k.zsh
      + builtins.readFile ./zshrc.zsh
      + ''
        # source ${pkgs.tmuxinator}/share/zsh/site-functions/_tmuxinator
        if [[ -e /run/secrets/env.json ]];then
          cat /run/secrets/env.json | ${pkgs.jq}/bin/jq -r 'to_entries | .[] | "export " + .key + "=" + (.value | @sh)' | source /dev/stdin
        fi
        if [[ -e /run/secrets-for-users/atuin-key ]];then
            nohup atuin login -u wangzi -p 03hat0zw0oEH7nipcKB6JqLpxptl7DdV -k $(cat /run/secrets-for-users/atuin-key) > /dev/null 2> /dev/null
        fi

        if [[ -r "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh" ]]; then
            source "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh"
        fi
        source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme

        PATH = "$PATH:$HOME/.local/bin:$HOME/.cargo/bin";
      '';
  };
  home.file.".config/atuin/config.toml".text =
    builtins.readFile ./atuin.toml
    + lib.optionalString pkgs.stdenv.isLinux ''
      key_path = "/run/secrets-for-users/atuin-key"
    '';
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };
  programs.carapace = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
  programs.yazi = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
  programs.direnv.enableZshIntegration = true;
  programs.command-not-found.enable = !config.programs.nix-index.enable;
  programs.nix-index = {
    enableZshIntegration = true;
  };
  home.activation.nix-index = lib.mkIf config.programs.nix-index.enable (
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [[ ! -e $HOME/.cache/nix-index/files ]]; then
       mkdir $HOME/.cache/nix-index/ -p || true
       nix-index &
      fi
    ''
  );
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  home.packages =
    with pkgs;
    (
      [
        ripgrep
        xclip
        iftop
        htop
        procs
        rmtrash
        bat
        eza
        du-dust
        duf
        tmuxinator
        tmux
        unstable.joshuto
      ]
      ++ (lib.optionals (pkgs.system == "x86_64-linux") [
        powertop
        iotop
      ])
    );
}
