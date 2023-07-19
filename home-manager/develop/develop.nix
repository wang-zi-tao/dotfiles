{ pkgs, config, lib, ... }:
let
  rust-env = pkgs.fenix.combine (with pkgs.fenix.complete; [
    cargo
    clippy
    rust-std
    rustc
    rustfmt
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
      pandas
      matplotlib
      django
      pygobject3
      ipython
      pylint
      jedi
      pip
      setuptools
      autopep8
    ]);
in
{
  imports = [
    ./git.nix
    ./cpp.nix
  ];
  programs.go = {
    enable = true;
    goBin = ".go/bin";
    goPath = ".go/path";
  };
  home.sessionVariables = with pkgs; {
    RUSTUP_DIST_SERVER = "http://mirrors.ustc.edu.cn/rust-static";
    RUSTUP_UPDATE_ROOT = "http://mirrors.ustc.edu.cn/rust-static/rustup";
    RUST_BACKTRACE = "1";
    PATH = "$PATH:$HOME/.local/bin:$HOME/.cargo/bin";
  };
  home.packages = with pkgs; [
    rnix-lsp
    nixfmt
    socat
    pandoc
    devtodo
    graphviz
    curlie
    highlight
    xlsx2csv

    pkg-config
    ctags
    global
    ninja
    meson
    rust-env

    fzf
    ptags
    global
    file

    unixtools.xxd
    gh

    yasm
    cargo-watch

    sumneko-lua-language-server
    nodePackages.typescript-language-server
    nodejs
    nodePackages.typescript
    nodePackages.pyright
    cmake-language-server
    lua5_4

    jdk
    maven
    gradle

    ctop
    # distant
    iperf2
    tokei
    nix-prefetch
    statix
    docker-compose
    k9s
    kubectl
    kubernetes-helm

    neovim-remote

    (python3.withPackages (ps:
      with ps; [
        debugpy
        # numpy
        # pandas
        # matplotlib
        # pip
        # setuptools
      ]))

    luajitPackages.luacheck
    luajitPackages.luarocks
    google-java-format
    stylua
    shfmt
    nodePackages.prettier
    shellcheck
    deno
    nodePackages.yaml-language-server

    cpulimit

  ];
  home.file = {
    ".cargo/config.toml".text = ''
      [target.x86_64-unknown-linux-gnu]
      linker = "${pkgs.clang_14}/bin/clang"
      rustflags = ["-C", "link-arg=--ld-path=${pkgs.mold}/bin/mold", "-L", "${pkgs.glibc}/lib/Scrt1.o"]
    '';
    ".gdbinit".text = ''
      set debuginfod enabled on
    '';
  };
  programs.zsh.shellAliases = {
    mvn = "unset JAVA_TOOL_OPTIONS && mvn";
    dc = "docker-compose";
    dcl = "docker-compose logs";
    dcb = "docker-compose build";
    dcd = "docker-compose down";
    dcu = "docker-compose up";
    dcud = "docker-compose up -d";
    diff = "nvr -s -d";
  };
}
