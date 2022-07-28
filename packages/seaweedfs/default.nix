{ lib
, fetchFromGitHub
, buildGoModule
, testVersion
, seaweedfs
, pkgs
, buildGo118Module
}:
buildGo118Module rec {
  pname = "seaweedfs";
  version = "3.18";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-YPtKawNKLoUG5/YgdU5bqT60FJ7zLm3JhqGtN90/Ztw=";
  };

  vendorSha256 = "sha256-zoNzis6xXCTyl7zBjjIRMNFueQehyQyk5q3Eb9PKOM0=";

  subPackages = [ "weed" ];

  passthru.tests.version =
    testVersion { package = seaweedfs; command = "weed version"; };

  meta = with lib; {
    description = "Simple and highly scalable distributed file system";
    homepage = "https://github.com/chrislusf/seaweedfs";
    mainProgram = "weed";
    license = licenses.asl20;
  };
}

