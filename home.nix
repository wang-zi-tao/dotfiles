{ config, pkgs, lib, ... }:

let
  scripts = with builtins;
    (map (f: pkgs.writeScriptBin f (readFile (./scripts + "/${f}")))
      (attrNames (readDir ./scripts)));
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
      setuptools
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
  programs.firefox = {
    enable = true;
    # package = pkgs.unstable.firefox;
  };
  services.unclutter.enable = true;
  xresources.properties = { "Xft.dpi" = 96; };
  gtk = {
    enable = true;
    theme.name = "Orchis-light";
    theme.package = pkgs.unstable.orchis-theme;
    iconTheme.name = "Tela-blue";
    iconTheme.package = pkgs.tela-icon-theme;
    font.package = pkgs.iosevka;
    font.name = "Iosevka Terminal";
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = false;
      gtk-button-images = true;
      gtk-cursor-theme-name = "Layan-white-cursors";
      gtk-cursor-theme-size = 24;
      gtk-decoration-layout = "close,maximize,minimize:";
      gtk-enable-animations = true;
      gtk-enable-event-sounds = 0;
      gtk-enable-input-feedback-sounds = 0;
      # gtk-font-name=Noto Sans,  10;
      gtk-menu-images = true;
      gtk-modules = "gail:atk-bridge:colorreload-gtk-module";
      gtk-primary-button-warps-slider = false;
      gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
      gtk-toolbar-style = 3;
      gtk-xft-antialias = 1;
      gtk-xft-hinting = 1;
      gtk-xft-hintstyle = "hintslight";
      gtk-xft-rgba = "rgb";
    };
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  home.file.".pip/pip.conf".text = ''
    [global]
    index-url = https://pypi.mirrors.ustc.edu.cn/simple
  '';
  home.sessionVariables = with pkgs; {
    XMODIFIERS = "@im=ibus";
    GTK_IM_MODULE = "ibus";
    EDITOR = "${neovim}/bin/nvim";
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
    PKG_CONFIG_PATH = "${openssl.dev}/lib/pkgconfig";
    # PATH = "$HOME/.local/bin:$HOME/.cargo/bin:$PATH";
    __NV_PRIME_RENDER_OFFLOAD = "1";
    __NV_PRIME_RENDER_OFFLOAD_PROVIDER = "NVIDIA-G0";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    __VK_LAYER_NV_optimus = "NVIDIA_only";
  };
  inherit imports;
  home.packages = with pkgs;
    scripts ++ [

      rnix-lsp 
      nixfmt
      feh
      pfetch
      neofetch
      killall
      x11docker
      xclip
      xdotool
      xorg.xbacklight
      socat

      jq
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
      kubectl
      k9s
      kubernetes-helm
      pandoc
      graphviz
      # ffmpeg
      meld
      devtodo
      lsof

      qv2ray
      v2ray
      wpsoffice
      # libreoffice
      postman
      gnome.gnome-nettool
      gnome.gnome-todo
      google-chrome
      dconf
      gnome.dconf-editor
      gnome.gnome-tweaks
      gnome.gpaste
      gnome.sushi
      gnome.nautilus
      gnome.seahorse

      libsForQt5.kdeconnect-kde
      # gimp
      # slack
      # inkscape
      # krita
      # shotcut
      drawio
      # lens
      nur.repos.linyinfeng.icalingua

      gnome.baobab
      gnome.cheese
      gnome.gedit
      gnome.gnome-calculator
      gnome.gnome-calendar
      # gnome.gnome-characters
      gnome.gnome-clocks
      # gnome.gnome-contacts
      # gnome.gnome-font-viewer
      # gnome.gnome-logs
      # gnome.gnome-maps
      gthumb
      gnome.gnome-music
      gnome.gnome-screenshot
      gnome.gnome-system-monitor
      gnome.nautilus
      pkgs.gnome-connections
      gnome.totem
      gnome.gnome-software
      dolphin

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

      nodejs
      nodePackages.typescript
      python3-env

      jdk
      maven
      gradle
    ];
}
