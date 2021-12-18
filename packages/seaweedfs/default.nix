{ stdenv, lib, fetchFromGitHub, buildGoModule, testVersion, seaweedfs, fuse }:

buildGoModule rec {
  pname = "seaweedfs";
  version = "2.82";

  src = fetchFromGitHub {
    owner = "chrislusf";
    repo = "seaweedfs";
    rev = version;
    sha256 = "sha256-stPtBLso55dl3/L1TcgXWk+jEBuQadQRXYAUAL45x5s=";
  };

  vendorSha256 = "sha256-beE48kOTr0LHfBxo9BovL1DvUJ2eg8ucuBoqen/OEZ8=";

  subPackages = [ "weed" ];

  passthru.tests.version = testVersion {
    package = seaweedfs;
    command = "weed version";
  };

  meta = with lib; {
    description = "Simple and highly scalable distributed file system";
    homepage = "https://github.com/chrislusf/seaweedfs";
    maintainers = with maintainers; [ cmacrae raboof ];
    license = licenses.asl20;
  };
}
