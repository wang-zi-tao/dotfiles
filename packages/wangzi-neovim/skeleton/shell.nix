{
  pkgs ? import <nixpkgs> { },
}:
let
  fenix = import (fetchTarball "https://github.com/nix-community/fenix/archive/main.tar.gz") { };
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ 
	# cmake
    # clang
	# clang-tools
  ];
  buildInputs = with pkgs; [ 

  ];
  shellHook = "";
  NIX_LD = "${pkgs.llvmPackages_18.libllvm.dev}/bin/ld.lld";
  NIX_LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [

  ];

}
