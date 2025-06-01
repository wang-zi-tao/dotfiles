{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  libvirt,
  libvirt-glib,
}:

rustPlatform.buildRustPackage rec {
  pname = "balloond";
  version = "0.1.3";

  src = fetchFromGitHub {
    owner = "wang-zi-tao";
    repo = pname;
    rev = version;
    sha256 = "sha256-wQ8Leqj8tdOuFjQpwWohPKwMFSW3UgQ0T1KkFTcT2iE=";
  };

  cargoHash = "sha256-Q1pIZy1E2w+5E10f7+TJn+v8xuDnDcWXlOwrpvW7jU0=";

  nativeBuildInputs = [
    pkg-config
    libvirt-glib.dev
  ];

  buildInputs = [ libvirt ];

  meta = with lib; {
    description = "auto balloon service for libvirt";
    homepage = "https://github.com/wang-zi-tao/balloond";
    license = licenses.mit;
  };
}
