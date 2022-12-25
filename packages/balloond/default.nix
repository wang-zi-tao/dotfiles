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
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "wang-zi-tao";
    repo = pname;
    rev = version;
    sha256 = "sha256-7OVlvVQnhUeUvKm2z0ZFb4KKmD/Hxy3MK8j2bxG72Z8=";
  };

  cargoSha256 = "sha256-OXyW63nQMrXJseV4wi5d9bD/C5S+tdIo+QUzn24O2LM=";

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
