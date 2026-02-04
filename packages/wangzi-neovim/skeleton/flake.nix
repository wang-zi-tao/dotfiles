{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # inputs.fenix.url = "github:nix-community/fenix";
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ 

            # (inputs.fenix.packages.${system}.fromToolchainFile {
            #   file = ./rust-toolchain.toml;
            #   sha256 = lib.fakeHash;
            # })
          ];
          nativeBuildInputs = with pkgs; [
			# cmake
    		# clang
			# clang-tools
          ];
  			shellHook = "";
  			NIX_LD = "${pkgs.llvmPackages_18.libllvm.dev}/bin/ld.lld";
  			NIX_LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [

  			];
        };
      });
}

