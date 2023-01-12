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
  version = "0.1.3";

  src = fetchFromGitHub {
    owner = "wang-zi-tao";
    repo = pname;
    rev = version;
    sha256 = "sha256-wQ8Leqj8tdOuFjQpwWohPKwMFSW3UgQ0T1KkFTcT2iE=";
  };

  cargoSha256 = "sha256-f1tN8D0EIDmZ6HUDm0wuSLmZxaN5cavByiZPEMmC73k=";

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
