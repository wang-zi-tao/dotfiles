{ lib
, fetchFromGitHub
, buildGoModule
, testVersion
, seaweedfs
, buildGo118Module
}:
buildGo118Module rec {
  pname = "seaweedfs";
  version = "3.37";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-VPwbFK6h88bDvWIqTnrn0kMvdqfVJ5cizH57hSCkGz8=";
  };

  vendorSha256 = "sha256-oZJGqtCdECafh3P47H4VYilAGlZinpE8A4o2AlV3DIo=";

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

