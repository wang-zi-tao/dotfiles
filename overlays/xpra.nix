pkgs: prev: {
  xpra = prev.xpra.overrideAttrs (oldAttrs: rec {
    pname = "xpra";
    version = "6.1.2";
    src = pkgs.fetchurl {
      url = "https://xpra.org/src/${pname}-${version}.tar.xz";
      hash = "sha256-BWf3nypfSrYCzpJ0OfBkecoHGbG1lEgu5jLZhfkIejQ=";
    };
    patches = [ ];
    buildInputs = oldAttrs.buildInputs ++ [ pkgs.xxHash ];
  });
}
