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
  version = "3.20";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-t6cYt+ROMivcOv4eTqNt7jkuu1j3EhPsaZZDIddrNnA=";
  };

  vendorSha256 = "sha256-GeL2I2pTlofbMV4XxC8ieARyMqBxz9/NorwBEQorV88=";

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

