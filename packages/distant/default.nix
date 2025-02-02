{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
  perl,
  stdenv,
}:

rustPlatform.buildRustPackage rec {
  pname = "distant";
  version = "v0.16.4";

  src = fetchFromGitHub {
    owner = "chipsenkbeil";
    repo = pname;
    rev = version;
    sha256 = "sha256-lCiTlyzp+q3NnwrILQZYM60fmbjfWFWYAy1rn7HqP54=";
  };

  cargoHash = "sha256-Yeht9Sv0/B1QC26jHMIfD2xmOtm48pZzxZXFcJ9rAXg=";

  nativeBuildInputs = [
    pkg-config
    perl
    openssl
    openssl.dev
  ];

  RUST_BACKTRACE = "full";

  cargoBuildFlags = [ "--offline" ];

  buildInputs = [ ];

  # requires unstable rust features
  RUSTC_BOOTSTRAP = 1;

  meta = with lib; {
    description = "ElKowars wacky widgets";
    homepage = "https://github.com/elkowar/eww";
    license = licenses.mit;
    broken = stdenv.isDarwin;
  };
}
