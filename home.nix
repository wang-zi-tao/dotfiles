{ config, pkgs, lib, ... }:

let
  python3-env = pkgs.python3.withPackages (ps:
    with ps; [
      numpy
      # pytorchWithCuda
      # tensorflowWithCuda
      pandas
      matplotlib
      django
      pygobject3
      ipython
      pylint
      jedi
      pip
      pylint-django
    ]);
  haskell-env = pkgs.haskellPackages.ghcWithHoogle (hp:
    with hp; [
      apply-refact
      haskell-language-server
      brittany
      cabal-install
      hlint
      xmobar
    ]);
  imports = [
    ./zsh/home.nix
    ./tmux/home.nix
    ./git/home.nix
    ./neovim/home.nix
    ./vscode/home.nix
    # ./dconf/home.nix
    ./alacritty/home.nix
    ./xmonad/home.nix
    ./xmonad/picom.nix
  ];
in {
  programs.home-manager.enable = true;

  neovim.IDE = true;
  # home.stateVersion = "21.11";
  # nixpkgs.config.allowUnfree = true;
  programs.command-not-found.enable = true;
  programs.firefox = { enableGnomeExtensions = true; };
  xresources.properties = { "Xft.dpi" = 96; };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  home.file.".envrc".text = ''
    use_nix
  '';
  home.file.".pip/pip.conf".text = ''
    [global]
    index-url = https://pypi.mirrors.ustc.edu.cn/simple
  '';
  xsession.profileExtra = ''
    export XMODIFIERS=@im=ibus
    export GTK_IM_MODULE="ibus"
    export JAVA_8_HOME=${pkgs.jdk8}
  '';
  inherit imports;
  # nixpkgs.overlays = (map (name: import (./overlays + "/${name}"))
    # (builtins.attrNames (builtins.readDir ./overlays))) ++ [
      # (final: prev: rec {
        # touchegg = prev.callPackage ./packages/touchegg { };
      # })
    # ];
  home.packages = with pkgs; [

    haskell-env
    # picom
    # nur.repos.reedrw.picom-next-ibhagwan
    feh
    pfetch
    neofetch
    killall
    x11docker
    dconf2nix
    xclip
    xdotool
    rofi
    polybar
    xorg.xbacklight

    direnv
    ranger
    atool
    poppler
    highlight
    catdoc
    catdocx
    xlsx2csv
    trash-cli
    sudo
    curl
    wget
    autojump
    zip
    unzip
    rnix-lsp
    nixfmt
    nix-du
    kubectl
    k9s
    kubernetes-helm
    pandoc
    graphviz

    orchis
    qv2ray
    v2ray
    wpsoffice
    # libreoffice
    postman
    gnome.gnome-nettool
    gnome.gnome-todo
    firefox
    google-chrome
    dconf
    gnome.dconf-editor
    gnome.gnome-tweaks
    gnome.gpaste
    lxappearance
    libsForQt5.kdeconnect-kde
    touchegg
    gimp
    slack
    inkscape
    krita
    shotcut
    drawio
    hugo
    lens
    octant
    octant-desktop
    gitkraken
    jetbrains.clion
    jetbrains.webstorm
    jetbrains.idea-community
    jetbrains.pycharm-community

    tela-icon-theme

    gnomeExtensions.gtile
    gnomeExtensions.runcat
    gnomeExtensions.ddterm
    gjs
    gnome.zenity
    vte
    # vte_290
    # gnomeExtensions.arcmenu
    gnomeExtensions.kimpanel
    gnomeExtensions.gsconnect
    gnomeExtensions.task-widget
    gnomeExtensions.blur-my-shell
    gnomeExtensions.autohide-battery
    gnomeExtensions.gtk-title-bar
    gnomeExtensions.hot-edge
    gnomeExtensions.net-speed-simplified
    gnomeExtensions.panel-scroll
    gnomeExtensions.proxy-switcher
    gnomeExtensions.screenshot-tool
    gnomeExtensions.task-widget
    gnomeExtensions.x11-gestures
    gnomeExtensions.system-monitor

    pkg-config
    ctags
    global
    gnumake
    ninja
    cmake
    clang
    clang-tools
    libcxx
    clang-analyzer
    ccls
    # gcc
    llvm
    rustup
    rust-analyzer
    rustracer

    fzf
    ptags
    global
    llvmPackages.bintools-unwrapped
    file

    # stylish-haskell
    go
    nodejs
    python3-env
    # python27Full
    lua
    rnix-lsp
    perl
    # ocaml

    openssl
    cryptodev

    jdk
    maven
    gradle
    kotlin
    scala
  ];
}
