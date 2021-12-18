{ pkgs, config, ... }: {
  imports = [
    ./tmux/tmux.nix
    ./zsh/zsh.nix
    ./procs/procs.nix
    ./htop.nix
    ./ranger.nix
    ../develop/git.nix
    ../develop/neovim/neovim.nix
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
  home.packages = with pkgs;
    scripts ++ [
      nix-index

      wcp

      killall
      atool
      bat
      choose
      ctop
      curl
      direnv
      duf
      dust
      exa
      fd
      jq
      lsof
      pistol
      poppler
      ps
      ripgrep
      sudo
      tldr-hs
      trash-cli
      wget
      xh
      zip
      unzip
      zoxide
      xclip
      pfetch
      neofetch
    ];
}
