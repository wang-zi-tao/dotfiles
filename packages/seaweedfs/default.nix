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
  version = "139e03";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = "139e039c4489d2f03f17ae3371defdf1609830f2";
    sha256 = "sha256-hFxoz0ypAEpgt1xsSsvgJUDs4w9s/NfGu68o9ZHGeyQ=";
  };

  vendorSha256 = "sha256-CrVZV+zxvvNQ5JPfmWnLCOFJ9XcQbFJVPaadfRGxq9k=";

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
