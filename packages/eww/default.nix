{ config, pkgs, lib, rustPlatform, fetchFromGitHub, pkg-config, gtk3, cairo
, naersk, system, pango, glib, ... }:

let
  rust-env = pkgs.fenix.combine (with pkgs.fenix.minimal; [
    cargo
    rust-std
    rustc
  ]);
  naersk-lib = (naersk.lib."${system}".override {
    cargo = rust-env;
    rustc = rust-env;
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

  cargoSha256 = "sha256-jDCLalMENNXAFo0iLWDeuXglnfOzDoQmGwlQPv+ndbw=";
  PKG_CONFIG_PATH = pkgs.lib.concatStrings (map (x: "${x.dev}/lib/pkgconfig:")
    (with pkgs; [ gtk3 glib pango cairo harfbuzz atk gdk-pixbuf ]));
  nativeBuildInputs = with pkgs; [ pkg-config gtk3 glib ];
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
