{ nixosConfig, pkgs, config, lib, ... }: {
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
    EDITOR = "${wangzi-neovim}/bin/nvim";
    VISUAL = "${wangzi-neovim}/bin/nvim";
    NIX_AUTO_RUN = "1";
    NIXPKGS_ALLOW_UNFREE = "1";
    PATH = "$PATH:/$HOME/.cargo/bin";
  };
  home.activation.neovim = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    rm $HOME/.cache/nvim/luacache $HOME/.cache/nvim/luacache_chunks $HOME/.cache/nvim/luacache_modpaths || true
  '';
  home.packages = with pkgs;
    scripts ++ [
      wangzi-neovim
      distant
      iperf2

      wcp

      nix-tree
      nload
      killall
      atool
      bat
      choose
      ctop
      curl
      direnv
      nix-prefetch
      (nix-direnv.override { enableFlakes = true; })
      duf
      exa
      fd
      jq
      lsof
      pistol
      poppler
      ps
      ripgrep
      sudo
      trash-cli
      wget
      xh
      zip
      unzip
      xclip
      pfetch
      neofetch
    ];
}
