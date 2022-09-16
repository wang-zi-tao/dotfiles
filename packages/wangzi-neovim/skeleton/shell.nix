{ pkgs ? import <nixpkgs> { } }:
let fenix = import (fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz") { };
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ ];
  shellHook = "";
} 

