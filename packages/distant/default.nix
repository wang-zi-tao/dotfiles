{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, stdenv
}:

rustPlatform.buildRustPackage
rec {
  pname = "distant";
  version = "2509f48";

  src = fetchFromGitHub {
    owner = "chipsenkbeil";
    repo = pname;
    rev = "2509f48d3e16c0b39f6f5a7fec0e25afcd79e75b";
    sha256 = "sha256-mp6uA3EJiFxdlsJtsSjn4QYvAnK4xwdLTGMig0RQvs0=";
  };

  cargoSha256 = "sha256-9f/7GHMDiEnCiOIbTwUwuw7dJ1KQJvdzWFj97/mJPXI=";

  nativeBuildInputs = [ pkg-config ];

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
