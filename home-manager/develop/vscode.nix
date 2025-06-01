{ pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.unstable.vscode;
    profiles.default.extensions = with pkgs.unstable.vscode-extensions; [
      # markdowm
      yzhang.markdown-all-in-one
      # sh
      mads-hartmann.bash-ide-vscode
      # nix
      bbenoist.nix
      jnoortheen.nix-ide
      brettm12345.nixfmt-vscode
      # go
      golang.go
      # java
      redhat.java
      scala-lang.scala
      # python
      ms-toolsai.jupyter
      ms-python.python
      ms-python.vscode-pylance
      # rust
      rust-lang.rust-analyzer
      # haskell
      justusadam.language-haskell
      # mark
      redhat.vscode-yaml
      dotjoshjohnson.xml
      tamasfe.even-better-toml
      mechatroner.rainbow-csv
      # C/C++
      vadimcn.vscode-lldb
      xaver.clang-format
      # misc
      gruntfuggly.todo-tree
      codezombiech.gitignore
      streetsidesoftware.code-spell-checker
      stephlin.vscode-tmux-keybinding
      ms-azuretools.vscode-docker
      ms-vscode-remote.remote-ssh
      formulahendry.code-runner
      formulahendry.auto-close-tag
      # theme
      mskelton.one-dark-theme
      pkief.material-icon-theme
    ];
  };
}
