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
    config.neovim.pkg
    gcc-unwrapped.lib
    neovim-remote
    bear
    cpplint
    # shfmt
    # shellcheck
    gef

    gcc
    gdb
    (pkgs.writeScriptBin "clang" "${clang}/bin/clang")
    (pkgs.writeScriptBin "clang++" "${clang}/bin/clang++")
    (pkgs.writeScriptBin "lldb" "${lldb}/bin/lldb")
  ];
  home.file.".gef.rc".text = ''
    [gef]
    autosave_breakpoints_file = "${config.home.homeDirectory}/.gef.breakpoints"

    [context]
    layout = -legend -regs -stack -code -args -threads trace extra memory source
    nb_lines_threads = 1
    nb_lines_code = 16
    nb_lines_code_prev = 2
    nb_lines_backtrace = 2
    
    [print-format]
    max_size_preview = 16

    [aliases]
  '';
  programs.nix-index.enable = true;
  # home.file.".config/nvim/parser/cpp.so".source = lib.mkDefault "${pkgs.unstable.tree-sitter.builtGrammars.tree-sitter-cpp}/parser";
}
