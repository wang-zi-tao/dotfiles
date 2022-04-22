{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, gtk3
, withWayland ? false
, gtk-layer-shell
, stdenv
}:

rustPlatform.buildRustPackage rec {
  pname = "eww";
  version = "fb0e57a";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = pname;
    rev = "fb0e57a0149904e76fb33807a2804d4af82350de";
    sha256 = "sha256-oAbB9aW/nqg02peqGEfETOGgeXarI6ZcAZ6DzDXbOSE=";
  };

  cargoSha256 = "sha256-f76UwQ7SiIXpW0k6ogZpmH7E8PF5rWYR9y7IodAp5FA=";

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ gtk3 ] ++ lib.optional withWayland gtk-layer-shell;

  buildNoDefaultFeatures = withWayland;
  buildFeatures = lib.optional withWayland "wayland";

  cargoBuildFlags = [ "--bin" "eww" ];

  cargoTestFlags = cargoBuildFlags;

  # requires unstable rust features
  RUSTC_BOOTSTRAP = 1;

  meta = with lib; {
    description = "ElKowars wacky widgets";
    homepage = "https://github.com/elkowar/eww";
    license = licenses.mit;
    maintainers = with maintainers; [ figsoda lom ];
    broken = stdenv.isDarwin;
  };
}
