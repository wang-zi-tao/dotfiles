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
  version = "0b0715";

  src = fetchFromGitHub {
    owner = "elkowar";
    repo = pname;
    rev = "0b0715fd505200db5954432b8a27ed57e3e6a72a";
    sha256 = "sha256-wtrq8crcN7fdNAkCqKHrPpptP4FOEQwReUnSFcCMQzs=";
  };

  cargoSha256 = "sha256-ddXhKi+Rs9HuXWE17bt5C/ZC7Yjd0NucFMZtKP2o4r0=";

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
