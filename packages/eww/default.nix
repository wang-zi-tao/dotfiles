{ config, pkgs, lib, rustPlatform, fetchFromGitHub, pkg-config, gtk3, cairo
, naersk, system, pango, glib, ... }:

let
  rust-env = pkgs.fenix.toolchainOf {
    channel = "nightly";
    date = "2021-08-29";
    sha256 = "sha256:04p16i813mib6i7srysirqrgh6yrqhz231nldllcxxbbhf9ck090";
  };
  naersk-lib = (naersk.lib."${system}".override {
    cargo = rust-env.cargo;
    rustc = rust-env.rustc;
  });
in naersk-lib.buildPackage rec {
  pname = "eww";
  version = "70285e0ebf928df4d78725dfb02d9eae2273630c";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = pname;
    rev = version;
    sha256 = "sha256-jweiNNW1XHo6PhUu+EI83yXzhB3L/C2F8E+C/KH6Crw=";
  };

  nativeBuildInputs = with pkgs; [ pkg-config gtk3.dev glib.dev ];
  postPatch = ''
    cargo version
  '';
  meta = with lib; {
    description = "ElKowar's wacky widgets";
    homepage = "https://github.com/elkowar/eww";
    license = licenses.mit;
    maintainers = [ ];
  };
}
