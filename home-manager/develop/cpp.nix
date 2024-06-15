{
  pkgs,
  config,
  lib,
  ...
}:
{
  lazyPackage = with pkgs; [
    cmake-language-server
    cpplint
    valgrind
    cmake
    ccls
    gef
    "/nixfs/flake/str/nixpkgs#clang/bin/clang"
    "/nixfs/flake/str/nixpkgs#clang/bin/clang++"
    "/nixfs/flake/str/nixpkgs#lldb/bin/lldb"
    "/nixfs/flake/str/nixpkgs#gdb/bin/gdb"
    "/nixfs/flake/str/nixpkgs#gdb/bin/gdbserver"
  ];
  home.packages = with pkgs; [
    gnumake
    clang-tools
    # clang
    #  libcxx
    #   libcxx.dev
    # clang-analyzer
    # ccls
    perf-tools
    gperftools

    unixtools.xxd

    config.neovim.pkg
    gcc-unwrapped.lib
    neovim-remote
    bear
    # shfmt
    # shellcheck
    gcc
  ];
  home.file.".gef.rc".text = ''
    [gef]
    autosave_breakpoints_file = "${config.home.homeDirectory}/.gef.breakpoints"

    [context]
    # layout = -legend -regs -stack -code -args -threads trace extra memory source
    layout = -legend regs stack code args source -threads -trace extra memory
    nb_lines_threads = 1
    nb_lines_code = 8
    nb_lines_code_prev = 2
    nb_lines_backtrace = 2

    [print-format]
    max_size_preview = 16

    [aliases]
  '';
  programs.nix-index.enable = true;
  # home.file.".config/nvim/parser/cpp.so".source = lib.mkDefault "${pkgs.unstable.tree-sitter.builtGrammars.tree-sitter-cpp}/parser";
}
