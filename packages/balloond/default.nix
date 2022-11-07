{ lib
, fetchFromGitHub
, buildGoModule
, seaweedfs
}:
buildGoModule rec {
  pname = "balloond";
  version = "48d742";

  src = fetchFromGitHub {
    owner = "SapphicCode";
    repo = "balloond";
    rev = "48d74209758f92bd7fcc4ecf3ce2cc5ad311ac01";
    sha256 = "sha256-n+v2KGzo7zHv19Qh1E4RaEjeVQ41MHWkVC396dxp50c=";
  };

  vendorSha256 = "sha256-Md/S6yBQ56rB0XWG3S2vlPhFcqP4Q7DM6uuW3dwOCb0=";

  subPackages = [ "cmd/balloond" ];

  buildPhase = ''
    go build github.com/Pandentia/balloond/cmd/balloond
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp balloond $out/bin
  '';
  meta = with lib; {
    description = "An automatic libvirt memory balloon daemon";
    homepage = "https://github.com/SapphicCode/balloond";
    license = licenses.bsd3;
  };
}

