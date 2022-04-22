{ pkgs, config,nixosConfig, ... }: {
  imports=[];
  programs.zsh = {
    enable = true;
    autocd = true;
    enableCompletion = true;
    #enableAutosuggestions = false;
    enableVteIntegration = true;
    # enableSyntaxHighlighting = true;
    oh-my-zsh = {
      enable = false;
      # theme = "agnoster";
      plugins = [
        "git"
        "extract"
        "colored-man-pages"
        "sudo"
        # "command-not-found"
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
      syntaxHighlighting.highlighters = ["main" "brackets" "line"];
      pmodules = [
        "git"
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
    };
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.2.0";
          sha256 = "1gfyrgn23zpwv1vj37gf28hf5z0ka0w5qm6286a7qixwv7ijnrx9";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.zsh-syntax-highlighting;
      }
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.4.0";
          sha256 = "0z6i9wjjklb4lvr7zjhbphibsyx51psv50gm07mbb0kj9058j6kc";
        };
      }
      {
        name = "zsh-history-substring-search";
        src = pkgs.zsh-history-substring-search;
      }
    ];
    shellAliases = (builtins.listToAttrs (builtins.map(name: {name=name;value="nix-shell -p ${name} --run ${name}";})
    ["gimp" "kdenlive" "inkscape" "krita" "blender"]) )//{
      grep = "${pkgs.ripgrep}/bin/rg --color=auto";
      xclip = "${pkgs.xclip}/bin/xclip -selection c";
      s = "sudo su";
      j = "z";

      gclone = "${pkgs.git}/bin/git clone";

      powertop = "sudo ${pkgs.powertop}/bin/powertop";
      iotop = "sudo ${pkgs.iotop}/bin/iotop";
      iftop = "sudo ${pkgs.iftop}/bin/iftop";
      # nix-gc = "sudo nix-collect-garbage -d";
      top = "${pkgs.htop}/bin/htop";
      htop = "${pkgs.htop}/bin/htop";
      ps = "${pkgs.procs}/bin/procs";

      rm = "${pkgs.rmtrash}/bin/rmtrash -Iv";
      del = "${pkgs.busybox}/bin/rm -v ";
      # mv = "${pkgs.rsync}/bin/rsync -avP --delete-delay";
      # mv-origin = "${pkgs.busybox}/bin/mv";
      bat = "${pkgs.bat}/bin/bat --theme=Coldark-Dark";
      cat = "${pkgs.bat}/bin/bat --theme=Coldark-Dark --pager=never";
      cat-origin = "${pkgs.busybox}/bin/cat";
      less = "${pkgs.bat}/bin/bat --theme=Coldark-Dark";
      man = ''
        MANPAGER="sh -c 'col -bx | ${pkgs.bat}/bin/bat --theme=Coldark-Dark -l man -p'" man'';

      ls = "${pkgs.exa}/bin/exa --icons";
      l = "${pkgs.exa}/bin/exa -la --icons";
      ll = "${pkgs.exa}/bin/exa --git -la --icons";

      du = "${pkgs.du-dust}/bin/dust";
      df = "${pkgs.duf}/bin/duf";

      mux = "${pkgs.tmuxinator}/bin/tmuxinator";
      tt = "${pkgs.tmux}/bin/tmux split -p 10";
      tsh = "${pkgs.tmux}/bin/tmux split -h";
      tsv = "${pkgs.tmux}/bin/tmux split -v";


      ".." = "cd ..";

    } // (if nixosConfig.cluster.nodeConfig.develop.enable then {
      mvn = "unset JAVA_TOOL_OPTIONS && ${pkgs.maven}/bin/mvn";
      dc = "${pkgs.docker-compose}/bin/docker-compose";
      dcl = "${pkgs.docker-compose}/bin/docker-compose logs";
      dcb = "${pkgs.docker-compose}/bin/docker-compose build";
      dcd = "${pkgs.docker-compose}/bin/docker-compose down";
      dcu = "${pkgs.docker-compose}/bin/docker-compose up";
      dcud = "${pkgs.docker-compose}/bin/docker-compose up -d";
      perf = "sudo ${pkgs.perf-tools}/bin/perf";
      diff = "${pkgs.neovim-remote}/bin/nvr -s -d";
    } else {});
    sessionVariables = {
      _ZO_EXCLUDE_DIRS = "/nix";
    };
    initExtraFirst = ''
      if [[ -r "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh" ]]; then
        source "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh"
      fi
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
    '';
    initExtra = builtins.readFile ./p10k.zsh +  builtins.readFile ./zshrc.zsh + ''
      # export FZF_DEFAULT_COMMAND="${pkgs.ag}/bin/ag -p ~/.gitignore -g """
      source ${pkgs.tmuxinator}/share/zsh/site-functions/_tmuxinator
    '';
  };
  # programs.command-not-found.enable = true;
  programs.nix-index = {
    enable=true;
    enableZshIntegration=true;
  };
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

}
