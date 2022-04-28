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

  vendorSha256 = "sha256-6VB6la9auuigFAYeK6VQqux05NiaG9FdN00jkHqv+M4=";
  # configurePhase = ''
  #   ls 
  #   go version
  #   cat go.mod
  # '';

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
