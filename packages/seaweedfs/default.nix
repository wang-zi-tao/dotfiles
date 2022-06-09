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
  version = "3.06";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-GlTCE2cBnU1Eu46MFiNhOJ6rXaa4HKd8svHccyJp7Fo=";
  };

  vendorSha256 = "sha256-EcTv0XqSlztFKvQjCuaoliPmZ/pQevWUOauJ8dJo3IQ=";

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
