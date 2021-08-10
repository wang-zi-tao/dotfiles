final: prev: rec {
  wpsoffice = prev.wpsoffice.overrideAttrs (old: rec {
    version = "11.1.0.10702";

    src = final.fetchurl {
      url =
        "https://wdl1.cache.wps.cn/wps/download/ep/Linux2019/10702/wps-office_11.1.0.10702_amd64.deb";
      sha256 =
        "189dc0cf621acaf84fcb92edcd9162685e552285e23d210c26b36de2ee53431d";
    };
    buildInputs = old.buildInputs ++ [ final.pkgs.llvmPackages_8.libcxx ];
    unvendoredLibraries = [
      # Have to use parts of the vendored qt4
      # "Qt"
      # "SDL2"
      # "bz2"
      # "avcodec"
      # "avdevice"
      # "avformat"
      # "avutil"
      # "swresample"
      # "swscale"
      # "jpeg"
      # "png"
      # File saving breaks unless we are using vendored llvmPackages_8.libcxx
      # "c++"
      # "ssl"
      # "crypto"
      # "nspr"
      # "nss"
      # "odbc"
      # "tcmalloc" # gperftools
    ];
    installPhase = ''
      prefix=$out/opt/kingsoft/wps-office
      mkdir -p $out
      cp -r opt $out
      cp -r usr/* $out
      for lib in $unvendoredLibraries; do
        echo $lib
        rm -v "$prefix/office6/lib$lib"*.so{,.*}
      done
      for i in wps wpp et wpspdf; do
        substituteInPlace $out/bin/$i \
          --replace /opt/kingsoft/wps-office $prefix
      done
      for i in $out/share/applications/*;do
        substituteInPlace $i \
          --replace /usr/bin $out/bin
      done
    '';
  });

}
