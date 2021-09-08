{ config, pkgs, lib, ... }:

let
  rust-env = pkgs.fenix.combine (with pkgs.fenix.complete; [
    cargo
    clippy-preview
    rust-std
    rustc
    rustfmt-preview
    rust-src
    rust-docs
    rust-analyzer-preview
    rust-analysis
    miri-preview
    rls-preview
  ]);
  python3-env = pkgs.python3.withPackages (ps:
    with ps; [
      pynvim
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
  python2-env = pkgs.python2.withPackages (ps:
    with ps;
    [
      # ipython
      # pylint
      pip
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
  ];
in {
  programs.home-manager.enable = true;

  neovim.IDE = true;
  programs.command-not-found.enable = true;
  programs.firefox = { enableGnomeExtensions = true; };
  services.unclutter.enable = true;
  xresources.properties = { "Xft.dpi" = 96; };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  home.file.".pip/pip.conf".text = ''
    [global]
    index-url = https://pypi.mirrors.ustc.edu.cn/simple
  '';
  home.sessionVariables = {
    XMODIFIERS = "@im=ibus";
    GTK_IM_MODULE = "ibus";
    JAVA_8_HOME = "${pkgs.jdk8}";
    EDITOR = "${pkgs.neovim}/bin/nvim";
    http_proxy = "http://127.0.0.1:8889";
    https_proxy = "http://127.0.0.1:8889";
    HTTP_PROXY = "http://127.0.0.1:8889";
    HTTPS_PROXY = "http://127.0.0.1:8889";
    ALL_PROXY = "socks5://127.0.0.1:1089";
    NO_PROXY =
      "localhost,127.0.0.1,10.96.0.0/12,192.168.99.0/24,192.168.39.0/24";
    CURL_NIX_FLAGS = "-x $http_proxy";
    RUSTUP_DIST_SERVER = "http://mirrors.ustc.edu.cn/rust-static";
    RUSTUP_UPDATE_ROOT = "http://mirrors.ustc.edu.cn/rust-static/rustup";
    RUST_BACKTRACE = "1";
    GOPATH = "~/工作空间/Go";
    NIX_AUTO_RUN = "1";
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
    PATH = "$HOME/.local/bin:$HOME/.cargo/bin:$PATH";
    __NV_PRIME_RENDER_OFFLOAD = "1";
    __NV_PRIME_RENDER_OFFLOAD_PROVIDER = "NVIDIA-G0";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __VK_LAYER_NV_optimus = "NVIDIA_only";
  };
  inherit imports;
  home.packages = with pkgs; [

    haskell-env
    feh
    pfetch
    neofetch
    killall
    x11docker
    dconf2nix
    xclip
    xdotool
    xorg.xbacklight

    direnv
    ranger
    pistol
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
    ffmpeg
    meld

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
    # wine
    # winetricks
    # jetbrains.clion
    # jetbrains.webstorm
    # jetbrains.idea-community
    # jetbrains.pycharm-community

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
    gnomeExtensions.quake-mode
    gnomeExtensions.runcat
    gnomeExtensions.middle-click-to-close-in-overview
    gnomeExtensions.proxy-switcher
    gnomeExtensions.appindicator
    gnomeExtensions.gnome-40-ui-improvements

    pkg-config
    ctags
    global
    gnumake
    ninja
    cmake
    clang_12
    clang-tools
    libcxx
    clang-analyzer
    ccls
    # gcc-unwrapped
    llvm
    # rustup
    # rust-analyzer
    # rustracer
    rust-env

    fzf
    ptags
    global
    llvmPackages.bintools-unwrapped
    file

    # stylish-haskell
    go
    nodejs
    nodePackages.typescript
    python3-env
    python2-env
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
