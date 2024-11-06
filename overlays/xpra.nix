final: prev: rec {
  xpra = prev.xpra.overrideAttrs(oldAttrs: {
    version = "6.1.2";
        src = fetchurl {
        url = "https://xpra.org/src/${pname}-${version}.tar.xz";
        hash = "sha256-BWg3nypfSrYCzpJ0OfBkecoHGbG1lEgu5jLZhfkIejQ=";
    };
  });
}
