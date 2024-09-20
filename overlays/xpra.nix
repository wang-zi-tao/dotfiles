pkgs: prev: {
  xpra = prev.xpra.overrideAttrs (oldAttrs: rec {
    pname = "xpra";
    version = "6.1.2";
    src = pkgs.fetchurl {
      url = "https://xpra.org/src/${pname}-${version}.tar.xz";
      hash = "sha256-GnSEZoMurBznWo0SSwc4g3FgBLYE/uimDpwZcENTOM4=";
    };
    patches = [ ];
    buildInputs = oldAttrs.buildInputs ++ [ pkgs.xxHash ];

    postInstall = oldAttrs.postInstall + ''
        cp -r ${pkgs.xpra-html5}/install $out/share/xpra/www
    '';
  });
}
