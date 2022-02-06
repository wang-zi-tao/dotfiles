{ nixosConfig, pkgs, config, lib, ... }: {
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
  home.sessionVariables = with pkgs; {
    EDITOR = "${unstable.neovim}/bin/nvim";
    NIX_AUTO_RUN = "1";
    NIXPKGS_ALLOW_UNFREE = "1";
  };
  home.packages = with pkgs;
    scripts ++ [

      wcp

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
