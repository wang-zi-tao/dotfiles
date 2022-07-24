{ lib, stdenv, rustPlatform, fetchFromGitHub, pkg-config, openssl, curl }:

rustPlatform.buildRustPackage rec {
  pname = "nix-index";
  version = "e7c66b";

  src = fetchFromGitHub {
    owner = "bennofs";
    repo = "nix-index";
    rev = "e7c66ba52fcfba6bfe51adb5400c29a9622664a2";
    sha256 = "sha256-aBlJcylH7/MDiu0RVEiUwV1XufGfVk4OvsFutImCszY=";
  };

  cargoSha256 = "sha256-65JtxKvY3UgiVv03SandBHDPapHz9W10ICF6A/rffmU=";

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ openssl curl ];

  doCheck = false;

  postInstall = ''
    mkdir -p $out/etc/profile.d
    cp ./command-not-found.sh $out/etc/profile.d/command-not-found.sh
    substituteInPlace $out/etc/profile.d/command-not-found.sh \
      --replace "@out@" "$out"
  '';

  meta = with lib; {
    description = "A files database for nixpkgs";
    homepage = "https://github.com/bennofs/nix-index";
    license = with licenses; [ bsd3 ];
    maintainers = with maintainers; [ bennofs ncfavier ];
  };
}
