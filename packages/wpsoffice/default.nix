{ lib, stdenv, fetchurl, autoPatchelfHook, dpkg, wrapGAppsHook, libsForQt5, xorg
, alsa-lib, atk, bzip2, cairo, cups, dbus, expat, mesa
, fontconfig, freetype, gdk-pixbuf, glib, gperftools, gtk2-x11, libICE, libpng12
, libSM, libtool, libuuid, libX11, libxcb, libXcomposite, libXcursor, libXdamage
, libXext, libXfixes, libXi, libxml2, libXrandr, libXrender, libXScrnSaver
, libXtst, nspr, nss, curl, pango, sqlite, unixODBC, xz, zlib, libcxx
, libusb1, SDL2, libudev-zero }:
stdenv.mkDerivation rec {
  pname = "wpsoffice";
  version = "12.1.0.17900";
  src = fetchurl {
    url =
      "http://47.83.14.140/s/mmC7LFp6qZXXmat/download/wps-office_12.1.0.17900_amd64.deb";
    sha256 = "sha256-RnJvu3J0N9z2Vt1w2rzBmLTUzizd06j53rBOSZyxwpg=";
  };
  unpackCmd = "dpkg -x $src .";
  sourceRoot = ".";

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

  # dontPatchELF = true;

  # wpsoffice uses `/build` in its own build system making nix things there
  # references to nix own build directory
  noAuditTmpdir = true;

  unvendoredLibraries = [
    # Have to use parts of the vendored qt4
    #"Qt"
    "SDL2"
    "jpeg"
    "stdc++"
    "curl"
    "nss"
    "odbc"
    "tcmalloc" # gperftools
  ];

  autoPatchelfIgnoreMissingDeps = [
    "libkappessframework.so"
    "libuof.so"
    "libavcodec.so.59"
    "libavformat.so.59"
    "libavutil.so.57"
    "libavdevice.so.59"
    "libswscale.so.6"
    "libswresample.so.4"
  ];

  buildInputs = with xorg; [
    alsa-lib
    atk
    bzip2
    cairo
    dbus.lib
    expat
    mesa
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
    libXv
    libpng12
    libtool
    libuuid
    libxcb
    libxml2
    xz
    nspr
    nss
    curl
    pango
    # nixpkgs-old.qt4
    libsForQt5.qt5.qtbase
    sqlite
    unixODBC
    zlib
    cups.lib
    libusb1
    libcxx
    SDL2
    libudev-zero
  ];

  buildPhase = ''
    dpkg -x $src .
  '';

  installPhase = ''
    prefix=$out/opt/kingsoft/wps-office
      mkdir -p $out
      cp -r opt $out
      cp -r usr/* $out
      for lib in $unvendoredLibraries; do
        echo $lib
        rm -v "$prefix/office6/lib$lib"*.so{,.*}
      done
      chmod +x $prefix/office6/lib*.so{,.*}
      patchelf \
        --force-rpath --add-needed libudev.so.1 \
        $prefix/office6/addons/cef/libcef.so
      for i in wps wpp et wpspdf; do
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
  preFixup = ''
    for f in "$out"/bin/*; do
      echo "Wrapping $f"
      wrapProgram "$f" \
        "''${gappsWrapperArgs[@]}" \
        "''${qtWrapperArgs[@]}" \
        --suffix LD_LIBRARY_PATH : "$runtimeLibPath"
    done
  '';
}
