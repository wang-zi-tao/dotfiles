{ pkgs, config, ... }: {
  home.packages = with pkgs; [
    devtodo

    gnumake
    cmake
    clang-tools
    # clang
    libcxx
    libcxx.dev
    # clang-analyzer
    # ccls
    perf-tools
    gperftools

    unixtools.xxd

    cmake-language-server
    (wangzi-neovim.override { enable-all = config.neovim.full; })
    neovim-remote
    # shfmt
    # shellcheck

    # wineWowPackages.stable
    # winetricks
    wine
    qt5.full
    qtcreator

    (pkgs.buildEnv {
      name = "cpp_compiler";
      paths = with pkgs;[
        lldb
        gdb
        gcc
        clang_12
        bintools-unwrapped
      ];
      ignoreCollisions = true;
    })
  ];
  programs.nix-index.enable = true;
}
