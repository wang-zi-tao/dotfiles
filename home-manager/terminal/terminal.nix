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
    ../../secret/secret-home.nix
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
    home.file.".ssh/id_rsa".source = config.secret.ssh-private-key;
    home.file.".ssh/id_rsa.pub".source = config.secret.ssh-public-key;
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
