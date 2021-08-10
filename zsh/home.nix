{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    #enableAutosuggestions = false;
    # enableSyntaxHighlighting = true;
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [
        "git"
        "extract"
        "colored-man-pages"
        "command-not-found"
        "python"
        "vi-mode"
        "autojump"
        "colorize"
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
    ];
    shellAliases = {
      v = "${pkgs.neovim}/bin/nvim";
      r = "${pkgs.ranger}/bin/ranger";
      # nvidia-run =
      # "__NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia";
      # nvidia = "sudo mhwd-gpu --setxorg /etc/X11/xorg.conf.nvidia";
      # intel = "sudo mhwd-gpu --setxorg /etc/X11/xorg.conf.intel";
      # intel-nvidia-prime =
      # "sudo mhwd-gpu --setxorg /etc/X11/xorg.conf.intel-nvidia-prime";
      grep = "${pkgs.busybox}/bin/grep --color=auto";
      screenfetch =
        "${pkgs.screenfetch}/bin/screenfetch|${pkgs.lolcat}/bin/lolcat";
      # vi = "nvim";
      # vim = "nvim";
      xclip = "${pkgs.xclip}/bin/xclip -selection c";
      ls = "${pkgs.lsd}/bin/lsd --group-dirs first";
      top = "${pkgs.htop}/bin/htop";
      htop = "${pkgs.htop}/bin/htop";
      rm = "${pkgs.rmtrash}/bin/rmtrash";
      rm-without-trash = "${pkgs.busybox}/bin/rm";
    };
    initExtra = builtins.readFile ./zshrc.zsh + ''
      export EDITOR=${pkgs.neovim}/bin/nvim
    '';
  };
}
