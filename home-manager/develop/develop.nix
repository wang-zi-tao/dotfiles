{ pkgs, config, ... }:
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
      setuptools
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
  home.file.".pip/pip.conf".text = ''
    [global]
    index-url = https://pypi.mirrors.ustc.edu.cn/simple
  '';
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
    httpie
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
    docker-compose
    k9s
    kubectl
    kubernetes-helm

    (wangzi-neovim.override { enable-all = config.neovim.full; })
    neovim-remote
    python2
    luajitPackages.luacheck
    luajitPackages.luarocks
    google-java-format
    stylua
    shfmt
    shellcheck
    deno
    nodePackages.live-server
    nodePackages.yaml-language-server

  ];
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
