{ lib
, fetchFromGitHub
, buildGoModule
, testVersion
, seaweedfs
, pkgs
}:
pkgs.unstable.buildGoModule rec {
  pname = "seaweedfs";
  version = "2.98";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-GUmMwZOgU29XZA1wXJPbbmfVsd6h0KliepSskYVUfRE=
error";
  };

  vendorSha256 = "sha256-7VO2sAgwU1pd+AJLgfYm3XZmQ1mLML33E0GMBmgsupc=";

  subPackages = [ "weed" ];

  passthru.tests.version =
    testVersion { package = seaweedfs; command = "weed version"; };

  meta = with lib; {
    description = "Simple and highly scalable distributed file system";
    homepage = "https://github.com/chrislusf/seaweedfs";
    maintainers = with maintainers; [ cmacrae raboof ];
    mainProgram = "weed";
    license = licenses.asl20;
  };
}
