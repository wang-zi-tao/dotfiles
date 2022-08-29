{ pkgs, config,lib, ... }: {
  imports=[];
  programs.zsh = {
    enable = true;
    autocd = true;
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
      syntaxHighlighting.highlighters = ["main" "brackets" "line"];
      pmodules = [
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
        autoStartRemote = true;
      };
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
    shellAliases = (builtins.listToAttrs (builtins.map(name: {name=name;value="nix-shell -p ${name} --run ${name}";}) [
    "gimp" "kdenlive" "inkscape" "krita" "blender"]))//{
      grep = "rg --color=auto";
      xclip = "xclip -selection c";
      s = "sudo su";
      j = "z";

      gclone = "git clone";

      powertop = "sudo powertop";
      iotop = "sudo iotop";
      iftop = "sudo iftop";
      # nix-gc = "sudo nix-collect-garbage -d";
      top = "htop";
      htop = "htop";
      ps = "procs";

      rm = "rmtrash -Iv";
      del = "command rm -v ";
      # mv = "rsync -avP --delete-delay";
      # mv-origin = "mv";
      bat = "bat --theme=Coldark-Dark";
      cat = "bat --theme=Coldark-Dark --pager=never";
      less = "bat --theme=Coldark-Dark";
      man = ''
        MANPAGER="sh -c 'col -bx | bat --theme=Coldark-Dark -l man -p'" man'';

      ls = "exa --icons";
      l = "exa -la --icons";
      ll = "exa --git -la --icons";

      du = "dust";
      df = "duf";

      mux = "tmuxinator";
      tt = "tmux split -p 10";
      tsh = "tmux split -h";
      tsv = "tmux split -v";


      ".." = "cd ..";

    };
    sessionVariables = config.home.sessionVariables//{
      _ZO_EXCLUDE_DIRS = "/nix";
    };
    initExtraFirst = ''
      if [[ -r "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh" ]]; then
        source "$HOME/.cache/p10k-instant-prompt-${config.home.username}.zsh"
      fi
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
    '';
    initExtra = builtins.readFile ./p10k.zsh +  builtins.readFile ./zshrc.zsh + ''
      # source ${pkgs.tmuxinator}/share/zsh/site-functions/_tmuxinator
      if [[ -e /run/secrets/shell/${config.home.username} ]];then
        source /run/secrets/shell/${config.home.username}
      fi
    '';
  };
  programs.command-not-found.enable = !config.programs.nix-index.enable;
  programs.nix-index = {
    enableZshIntegration=true;
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
  home.packages = with pkgs;[ ripgrep xclip powertop iotop iftop htop procs rmtrash bat exa du-dust duf tmuxinator tmux ];
}
