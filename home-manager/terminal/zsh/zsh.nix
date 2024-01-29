{ pkgs, config, lib, ... }: {
  imports = [ ];
  programs.zsh = {
    enable = true;
    autocd = true;
    enableAutosuggestions = true;
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
      syntaxHighlighting.highlighters = [ "main" "brackets" "line" ];
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

      rm = "rmtrash -I";
      # mv = "rsync -avP --delete-delay";
      # mv-origin = "mv";
      bat = "bat --theme=Coldark-Dark";
      cat = "bat --theme=Coldark-Dark --pager=never";
      less = "bat --theme=Coldark-Dark";
      man = ''
        MANPAGER="sh -c 'col -bx | bat --theme=Coldark-Dark -l man -p'" man'';

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

    };
    sessionVariables = config.home.sessionVariables // {
      _ZO_EXCLUDE_DIRS = "/nix";
    };
    initExtraFirst = ''
      if [[ -r "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh" ]]; then
        source "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh"
      fi
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
    '';
    initExtra = builtins.readFile ./p10k.zsh + builtins.readFile ./zshrc.zsh + ''
      # source ${pkgs.tmuxinator}/share/zsh/site-functions/_tmuxinator
      if [[ -e /run/secrets/shell/${config.home.username} ]];then
        source /run/secrets/shell/${config.home.username}
      fi
    '';
  };
  programs.nushell = {
    enable = true;
    configFile.text = ''
    '';
  };
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
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
  programs.zoxide.enableNushellIntegration = true;
  programs.direnv.enableNushellIntegration = true;
  programs.direnv.enableZshIntegration = true;
  programs.keychain.enableNushellIntegration = true;
  programs.command-not-found.enable = !config.programs.nix-index.enable;
  programs.nix-index = {
    enableZshIntegration = true;
  };
  home.activation.nix-index = lib.mkIf config.programs.nix-index.enable (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [[ ! -e $HOME/.cache/nix-index/files ]]; then
     mkdir $HOME/.cache/nix-index/ -p || true
     nix-index &
    fi
  '');
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };
  home.packages = with pkgs;[ ripgrep xclip powertop iotop iftop htop procs rmtrash bat eza du-dust duf tmuxinator tmux joshuto ];
}
