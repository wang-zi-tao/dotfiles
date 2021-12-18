{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    #enableAutosuggestions = false;
    enableVteIntegration = true;
    # enableSyntaxHighlighting = true;
    oh-my-zsh = {
      enable = true;
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
    shellAliases = {
      v = "${pkgs.neovim}/bin/nvim";
      grep = "${pkgs.ripgrep}/bin/rg --color=auto";
      fd = "${pkgs.fd}/bin/fd --ignore-file='.snapshots'";
      xclip = "${pkgs.xclip}/bin/xclip -selection c";
      s = "sudo su";
      j = "z";

      dc = "${pkgs.docker-compose}/bin/docker-compose";
      dcl = "${pkgs.docker-compose}/bin/docker-compose logs";
      dcb = "${pkgs.docker-compose}/bin/docker-compose build";
      dcd = "${pkgs.docker-compose}/bin/docker-compose down";
      dcu = "${pkgs.docker-compose}/bin/docker-compose up";
      dcud = "${pkgs.docker-compose}/bin/docker-compose up -d";

      gclone = "${pkgs.git}/bin/git clone";
      diff = "${pkgs.neovim-remote}/bin/nvr -s -d";

      perf = "sudo perf";
      powertop = "sudo ${pkgs.powertop}/bin/powertop";
      iotop = "sudo ${pkgs.iotop}/bin/iotop";
      iftop = "sudo ${pkgs.iftop}/bin/iftop";
      # nix-gc = "sudo nix-collect-garbage -d";
      top = "${pkgs.htop}/bin/htop";
      htop = "${pkgs.htop}/bin/htop";
      ps = "${pkgs.procs}/bin/procs";

      # rm = "${pkgs.rmtrash}/bin/rmtrash -Iv";
      del = "${pkgs.busybox}/bin/rm -v ";
      # cp = "${pkgs.busybox}/bin/cp -v";
      bat = "${pkgs.bat}/bin/bat --theme=Coldark-Dark";
      cat = "${pkgs.bat}/bin/bat --theme=Coldark-Dark --pager=never";
      cat-origin = "${pkgs.busybox}/bin/cat";
      less = "${pkgs.bat}/bin/bat --theme=Coldark-Dark";
      man = ''
        MANPAGER="sh -c 'col -bx | ${pkgs.bat}/bin/bat --theme=Coldark-Dark -l man -p'" man'';

      ls = "${pkgs.exa}/bin/exa --icons";
      l = "${pkgs.exa}/bin/exa -la --icons";
      ll = "${pkgs.exa}/bin/exa -l --icons";

      du = "${pkgs.du-dust}/bin/dust";
      df = "${pkgs.duf}/bin/duf";

      mux = "${pkgs.tmuxinator}/bin/tmuxinator";
      tt = "${pkgs.tmux}/bin/tmux split -p 10";
      tsh = "${pkgs.tmux}/bin/tmux split -h";
      tsv = "${pkgs.tmux}/bin/tmux split -v";

      mvn = "unset JAVA_TOOL_OPTIONS && ${pkgs.maven}/bin/mvn";

    };
    initExtraFirst =
      "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
    initExtra = builtins.readFile ./zshrc.zsh + ''
      export FZF_DEFAULT_COMMAND="${pkgs.ag}/bin/ag -p ~/.gitignore -g """
      source ${pkgs.tmuxinator}/share/zsh/site-functions/_tmuxinator
    '';
  };
  home.file.".p10k.zsh".source = ./p10k.zsh;
  # programs.autojump.enable = true;
  programs.command-not-found.enable = true;
}
