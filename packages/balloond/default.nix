{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, libvirt
, libvirt-glib
}:

rustPlatform.buildRustPackage
rec {
  pname = "balloond";
  version = "0.1.2";

  src = fetchFromGitHub {
    owner = "wang-zi-tao";
    repo = pname;
    rev = version;
    sha256 = "sha256-quqgq+DU9zRSjw0JNL/rCS7WkPmKyes/CbE52NawWeo=";
  };

  cargoSha256 = "sha256-gnl9PQF6GTxWMauLg6Wopeaek9iAeTIqkZ9gCfrKo1g=";

  nativeBuildInputs = [
    pkg-config
    libvirt-glib.dev
  ];

  buildInputs = [
    libvirt
  ];

  meta = with lib; {
    description = "auto balloon service for libvirt";
    homepage = "https://github.com/wang-zi-tao/balloond";
    license = licenses.mit;
  };
}
