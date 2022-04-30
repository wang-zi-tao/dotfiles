{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, openssl
, perl
, stdenv
}:

rustPlatform.buildRustPackage
rec {
  pname = "distant";
  version = "2022.3.31";

  src = fetchFromGitHub {
    owner = "chipsenkbeil";
    repo = pname;
    rev = "268ec948d602f9ffe03ce959cc2c6c3cf8defa99";
    sha256 = "sha256-ccZfQPvBSNMDyeMbGVshotU9hQ6jGl3/DHwr3OQ3nyY=";
  };

  cargoSha256 = "sha256-1IVJKiLHh0DbXv7U9H41FxhbFgFGLAaYY8ynonSI6vM=";

  nativeBuildInputs = [ pkg-config perl openssl openssl.dev ];

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
