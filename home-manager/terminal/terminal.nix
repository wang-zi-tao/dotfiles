{ pkgs, config, lib, ... }: {
  imports = [
    ./tmux/tmux.nix
    ./zsh/zsh.nix
    ./procs/procs.nix
    ./htop.nix
    ./ranger/ranger.nix
    ../develop/git.nix
  ];
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };
  manual.manpages.enable = true;
  home.sessionVariables = with pkgs; {
    EDITOR = "nvim";
    VISUAL = "nvim";
    NIX_AUTO_RUN = "1";
    NIXPKGS_ALLOW_UNFREE = "1";
    PATH = "$PATH:/$HOME/.cargo/bin";
  };
  home.activation.neovim = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    [[ -f $HOME/.cache/nvim/luacache ]] && rm $HOME/.cache/nvim/luacache || true
    [[ -f $HOME/.cache/nvim/luacache_chunks ]] && rm $HOME/.cache/nvim/luacache_chunks || true
    [[ -f $HOME/.cache/nvim/luacache_modpaths ]] && rm $HOME/.cache/nvim/luacache_modpaths || true
  '';
  home.packages = with pkgs;
    scripts ++ [
      neovim-remote
      (wangzi-neovim.override { enable-all = config.home.username == "wangzi"; })
      # distant
      iperf2


      lm_sensors
      nix-tree
      nload
      killall
      atool
      bat
      choose
      ctop
      nmap
      curl
      direnv
      nix-prefetch
      (nix-direnv.override { enableFlakes = true; })
      duf
      exa
      fd
      jq
      tldr
      just
      tokei
      lsof
      pistol
      poppler
      ps
      ripgrep
      silver-searcher
      choose
      sd
      sudo
      trash-cli
      wget
      xh
      zip
      unzip
      xclip
      pfetch
      watchexec
      neofetch
      gzip
      openssh
    ];
  home.file.".code-server/bin/node" = { source = "${pkgs.nodejs-16_x}/bin/node"; executable = true; };
}
