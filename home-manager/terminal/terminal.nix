{ pkgs, config, lib, ... }: {
  options.hostname = lib.mkOption { type = lib.types.str; };
  imports = [
    ./tmux/tmux.nix
    ./zsh/zsh.nix
    ./procs/procs.nix
    ./htop.nix
    ./ranger.nix
    ../develop/git.nix
    ../develop/neovim/neovim.nix
    ../../secrecy/secrecy.nix
  ];
  config = {
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };
    home.file.".ssh/id_rsa".source = config.srcrecy.ssh-private-key-of
      (if config.home.username != "root" then
        config.home.username
      else
        config.hostname);
    home.file.".ssh/id_rsa.pub".source = config.srcrecy.ssh-private-key-of
      (if config.home.username != "root" then
        config.home.username
      else
        config.hostname);
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
        nix-direnv
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
        xclip
        pfetch
        neofetch
      ];
  };
}
