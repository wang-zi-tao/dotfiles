{ lib
, fetchFromGitHub
, buildGoModule
, testVersion
, seaweedfs
, pkgs
}:
pkgs.unstable.buildGo118Module rec {
  pname = "seaweedfs";
  version = "3.00";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-XsD3IMcPJVBQpV26OjNRCMgH7PZjMKd4AOlfs2UQMkA=";
  };

  vendorSha256 = "sha256-ZM0poWj3wbYA/7rWSvBTMGhTs15fWwYAy8kQbFNVdY8=";

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
