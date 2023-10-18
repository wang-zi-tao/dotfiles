{ pkgs, config, lib, ... }: {
  home.packages = with pkgs; [
    devtodo

    gnumake
    cmake
    clang-tools
    valgrind
    # clang
    #  libcxx
    #   libcxx.dev
    # clang-analyzer
    # ccls
    perf-tools
    gperftools

    unixtools.xxd

    cmake-language-server
    (wangzi-neovim.override { enable-all = config.neovim.full; })
    gcc-unwrapped.lib
    neovim-remote
    bear
    cpplint
    # shfmt
    # shellcheck
    gef

    (pkgs.buildEnv {
      name = "cpp_compiler";
      paths = with pkgs;[
        clang
        lldb
        gcc
        gdb
        bintools-unwrapped
      ];
      ignoreCollisions = true;
    })
  ];
  programs.nix-index.enable = true;
  # home.file.".config/nvim/parser/cpp.so".source = lib.mkDefault "${pkgs.unstable.tree-sitter.builtGrammars.tree-sitter-cpp}/parser";
}
