final: prev: rec {
  touchegg = prev.touchegg.overrideAttrs (old:
    with final.pkgs; rec {
      version = "2.0.11";
      src = fetchurl {
        url =
          "https://github.com/JoseExposito/touchegg/archive/refs/tags/2.0.11.tar.gz";
        sha256 =
          "6b8848d2948eed7558cf4bb72748f5c9f423de8a019feacfb690dfe9162b5dce";
      };

      buildInputs = [ xorg.libX11 xorg.libXtst gtk3 cairo pugixml libinput ];

      nativeBuildInputs = [ cmake pkg-config ];

      preConfigure = ''
        sed -e "s/NOT DEFINED USE_SYSTEMD OR USE_SYSTEMD/0/g" -i ./CMakeLists.txt
      '';
    });

}
