{ lib
, stdenv
, fetchurl
, autoPatchelfHook
, dpkg
, wrapGAppsHook
, libsForQt5
, xorg
, alsa-lib
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
, gperftools
, gtk2-x11
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
, libXtst
, nspr
, nss
, openssl
, pango
, sqlite
, unixODBC
, xz
, zlib
, libcxxabi
, libcxx
, qt4
, zulip
, steam
}:
stdenv.mkDerivation rec {
  pname = "xiezuo";
  version = "3.16.0";
  src = fetchurl {
    url =
      "http://47.243.22.114/s/wY95ciKkQCGGrGz/download/xiezuo_3.16.0_amd64-stable-3.16.0.deb";
    sha256 = "sha256-ExGajbmjoSZAnMwNlb961imAbUJ6tFCuRQBYTJPoBFo=";
  };
  unpackCmd = "dpkg -x $src .";
  sourceRoot = ".";

  nativeBuildInputs =
    [ autoPatchelfHook dpkg wrapGAppsHook libsForQt5.qt5.wrapQtAppsHook ];

  meta = with lib; {
    description = "金山协作客户端";
    homepage = "https://xz.wps.cn/";
    platforms = [ "x86_64-linux" ];
    hydraPlatforms = [ ];
    license = licenses.unfreeRedistributable;
  };


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
  autoPatchelfIgnoreMissingDeps = [ ];
  buildInputs = with xorg; [
    alsa-lib
    atk
    bzip2
    cairo
    dbus.lib
    expat
    ffmpeg
    fontconfig
    freetype
    gdk-pixbuf
    glib
    gperftools
    gtk2-x11
    libICE
    libSM
    libX11
    libXScrnSaver
    libXcomposite
    libXcursor
    libXdamage
    libXext
    libXfixes
    libXi
    libXrandr
    libXrender
    libXtst
    libpng12
    libtool
    libuuid
    libxcb
    libxml2
    xz
    nspr
    nss
    openssl
    pango
    qt4
    libsForQt5.qt5.qtbase
    sqlite
    unixODBC
    zlib
    cups.lib
    libcxxabi
    libcxx

    xorg.libxshmfence
    libsForQt5.fcitx-qt5
  ];
  libPath = with xorg;
    lib.makeLibraryPath (buildInputs ++ [ ]);

  installPhase =
    let
      steam-run = (steam.override {
        extraPkgs = p: buildInputs;
      }).run;
    in
    ''
      prefix=$out/opt/xiezuo
      mkdir -p $out
      cp -r opt $out
      cp -r usr/* $out
      patchelf \
        --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --force-rpath --set-rpath "${stdenv.cc.cc.lib}/lib64:${libPath}:$(patchelf --print-rpath $prefix/xiezuo)" \
        $prefix/xiezuo
      mkdir $out/bin
      makeWrapper ${steam-run}/bin/steam-run $out/bin/xiezuo \
        --add-flags $prefix/xiezuo \
        --argv0 /opt/xiezuo/xiezuo
      substituteInPlace $out/share/applications/xiezuo.desktop \
        --replace /opt/xiezuo/xiezuo $out/bin/xiezuo
    '';

  runtimeLibPath = lib.makeLibraryPath [ cups.lib ];

  dontWrapQtApps = true;
  dontWrapGApps = true;
}
