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
  version = "3.32";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-GMOLlkBfY3ShVojdRrmpMYgoea52kq4aXr/oZj5bJWo=";
  };

  vendorSha256 = "sha256-cEzPKx54rssyAytYenIcud3K0f7xuO8WzE8wdMqZipE=";

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

