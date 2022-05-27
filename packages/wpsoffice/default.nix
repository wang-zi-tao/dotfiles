{ lib
, stdenv
, fetchurl
, autoPatchelfHook
, dpkg
, wrapGAppsHook
, libsForQt5
, xorg
, alsa-lib
, alsaLib
, atk
, bzip2
, cairo
, cups
, dbus
, expat
, ffmpeg
, fontconfig
, freetype
, gdk-pixbuf
, glib
, glui
, gperftools
, gtk2-x11
, libGLU
, libICE
, libpng12
, libSM
, libtool
, libuuid
, libX11
, libxcb
, libXcomposite
, libXcursor
, libXdamage
, libXext
, libXfixes
, libXi
, libxml2
, libXrandr
, libXrender
, libXScrnSaver
, libxslt
, libXtst
, lzma
, nspr
, nss
, openssl
, pango
, qt4
, sqlite
, unixODBC
, xz
, zlib
, zotero
}:
stdenv.mkDerivation rec {
  pname = "wpsoffice";
  version = "11.1.0.10920";
  src = fetchurl {
    url =
      "https://wps-linux-personal.wpscdn.cn/wps/download/ep/Linux2019/10976/wps-office_11.1.0.10976_amd64.deb";
    sha256 = "sha256-GndezCYqIdTRJ4TV5CS5JP9HX+xjpDNeuZjENJLs0g0=";
  };
  unpackCmd = "dpkg -x $src .";
  sourceRoot = ".";

  postUnpack = lib.optionalString (version == "11.1.0.9505") ''
    # distribution is missing libjsapiservice.so, so we should not let
    # autoPatchelfHook fail on the following dead libraries
    rm opt/kingsoft/wps-office/office6/{libjsetapi.so,libjswppapi.so,libjswpsapi.so}
  '';

  nativeBuildInputs =
    [ autoPatchelfHook dpkg wrapGAppsHook libsForQt5.qt5.wrapQtAppsHook ];

  meta = with lib; {
    description = "Office suite, formerly Kingsoft Office";
    homepage = "https://www.wps.com/";
    platforms = [ "x86_64-linux" ];
    hydraPlatforms = [ ];
    license = licenses.unfreeRedistributable;
    maintainers = with maintainers; [ mlatus th0rgal ];
  };

  buildInputs = with xorg; [
    alsa-lib
    alsaLib
    atk
    bzip2
    cairo
    cups
    dbus.daemon.lib
    dbus.lib
    expat
    ffmpeg
    fontconfig
    fontconfig.lib
    freetype
    gdk-pixbuf
    glib
    glui
    gperftools
    gtk2-x11
    libGLU
    libICE
    libpng12
    libsForQt5.qt5.qtbase
    libSM
    libtool
    libuuid
    libX11
    libxcb
    libXcomposite
    libXcursor
    libXdamage
    libXext
    libXfixes
    libXi
    libxml2
    libXrandr
    libXrender
    libXScrnSaver
    libxslt
    libXtst
    lzma
    nspr
    nss
    openssl
    pango
    qt4
    sqlite
    unixODBC
    xz
    zlib
    zotero
  ];

  dontPatchELF = true;

  # wpsoffice uses `/build` in its own build system making nix things there
  # references to nix own build directory
  noAuditTmpdir = true;

  unvendoredLibraries = [
    # Have to use parts of the vendored qt4
    #"Qt"
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
    # "c++"
    # "ssl"
    # "crypto"
    # "nspr"
    # "nss"
    # "odbc"
    # "tcmalloc" # gperftools
  ];
  libPath = with xorg;
    lib.makeLibraryPath [
      libX11
      libpng12
      glib
      libSM
      libXext
      fontconfig
      zlib
      freetype
      libICE
      cups
      libXrender
      libxcb

      alsaLib
      atk
      cairo
      dbus.daemon.lib
      expat
      fontconfig.lib
      gdk-pixbuf
      gtk2-x11
      lzma
      pango
      zotero
      sqlite
      libuuid
      libXcomposite
      libXcursor
      libXdamage
      libXfixes
      libXi
      libXrandr
      libXScrnSaver
      libXtst
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
        patchelf \
          --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
          --force-rpath --set-rpath "${stdenv.cc.cc.lib}/lib64:${libPath}:$(patchelf --print-rpath $prefix/office6/$i)" \
          $prefix/office6/$i
        substituteInPlace $out/bin/$i \
          --replace /opt/kingsoft/wps-office $prefix
      done
      for i in $out/share/applications/*;do
        substituteInPlace $i \
          --replace /usr/bin $out/bin
      done
  '';

  runtimeLibPath = lib.makeLibraryPath [ cups.lib ];

  dontWrapQtApps = true;
  dontWrapGApps = true;
  postFixup = ''
    for f in "$out"/bin/*; do
      echo "Wrapping $f"
      wrapProgram "$f" \
        "''${gappsWrapperArgs[@]}" \
        "''${qtWrapperArgs[@]}" \
        --suffix LD_LIBRARY_PATH : "$runtimeLibPath"
    done
  '';
}
