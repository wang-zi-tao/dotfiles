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
  lazyPackage = with pkgs; [
    "/nixfs/flake/str/nixpkgs#jdk/bin/java"
    "/nixfs/flake/str/nixpkgs#jdk/bin/javac"
    "/nixfs/flake/str/nixpkgs#jdk/bin/jar"
    "/nixfs/flake/str/nixpkgs#jdk/bin/jshell"
    "/nixfs/flake/str/nixpkgs#jdk/bin/jdb"
    jdk
    ghc
    "/nixfs/flake/str/nixpkgs#ghc/bin/ghci"

    "/nixfs/flake/str/nixpkgs#scala/bin/scala"
    "/nixfs/flake/str/nixpkgs#scala/bin/scalac"
    "/nixfs/flake/str/nixpkgs#scala/bin/scalap"

    kubectl
    k9s
    kubernetes-helm
    nix-prefetch
    docker-compose
    cpulimit
    ctop

    bison

    yasm
    xlsx2csv
    pandoc
    socat
    devtodo
    tokei
    ctags
    "/nixfs/flake/str/nixpkgs#lldb/bin/lldb"
    "/nixfs/flake/str/nixpkgs#lldb/bin/lldb-vscode"
    "/nixfs/flake/str/nixpkgs#lldb/bin/lldb-server"

    "rnix-lsp"
    "nixfmt"
    google-java-format
    stylua
    shfmt
    shellcheck
    deno
    vala-language-server
    cmake-language-server
    "haskellPackages.haskell-debug-adapter"
    "haskellPackages.haskell-language-server"
    "nodePackages.typescript-language-server"
    "nodePackages.yaml-language-server"
    "nodePackages.prettier"
    "luajitPackages.luacheck"
    "luajitPackages.luarocks"
    "nodePackages.pyright"
  ];
  home.packages = with pkgs; [
    graphviz
    curlie
    highlight

    pkg-config
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

    cargo-watch

    sumneko-lua-language-server
    nodejs
    nodePackages.typescript

    lua5_4

    maven
    gradle

    # distant
    iperf2
    statix

    neovim-remote
    sccache

    (python3.withPackages (ps:
      with ps; [
        debugpy
        numpy
        pandas
        # matplotlib
        # pip
        # setuptools
      ]))

  ];
  home.file = {
    ".cargo/config.toml".text = ''
      [target.x86_64-unknown-linux-gnu]
      linker = "${pkgs.clang_14}/bin/clang"
      rustflags = ["-C", "link-arg=--ld-path=${pkgs.mold}/bin/mold", "-L", "${pkgs.glibc}/lib/"]
      [build]
      rustc-wrapper = "${pkgs.sccache}/bin/sccache"
    '';
    ".gdbinit".text = ''
      set debuginfod enabled on
      define add-symbol-file-auto
          # Parse .text address to temp file
          shell echo set \$text_address=$(readelf -WS $arg0 | grep .text | awk '{ print "0x"$5 }') >/tmp/temp_gdb_text_address.txt

          # Source .text address
          source /tmp/temp_gdb_text_address.txt

          #  Clean tempfile
          shell rm -f /tmp/temp_gdb_text_address.txt

          # Load symbol table
          add-symbol-file $arg0 $text_address
      end
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
