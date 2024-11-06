{ lib, stdenv, fetchurl, autoPatchelfHook, dpkg, wrapGAppsHook, libsForQt5, xorg
, alsa-lib, atk, bzip2, cairo, cups, dbus, expat, ffmpeg, rigsofrods-bin, mesa
, fontconfig, freetype, gdk-pixbuf, glib, gperftools, gtk2-x11, libICE, libpng12
, libSM, libtool, libuuid, libX11, libxcb, libXcomposite, libXcursor, libXdamage
, libXext, libXfixes, libXi, libxml2, libXrandr, libXrender, libXScrnSaver
, libXtst, nspr, nss, curl, pango, sqlite, unixODBC, xz, zlib, libcxxabi, libcxx
, nixpkgs-old, steam, libusb1, }:
stdenv.mkDerivation rec {
  pname = "wpsoffice";
  version = "12.1.0.17885";
  src = fetchurl {
    url =
      "https://wps-linux-personal.wpscdn.cn/wps/download/ep/Linux2023/17885/wps-office_12.1.0.17885_amd64.deb?t=1730810635&k=5bccef95e85b08b6ca152dea63cbe5eb";
    sha256 = "sha256-tCffhevxBnpNin1qBB2PTyOPpGvPLlI0qWLCCykltiU=";
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
    "avcodec"
    "avdevice"
    "avformat"
    "avutil"
    "swresample"
    "swscale"
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
  ];

  buildInputs = with xorg; [
    alsa-lib
    atk
    bzip2
    cairo
    dbus.lib
    expat
    ffmpeg.lib
    "${rigsofrods-bin}/share/rigsofrods/"
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
    # libcxxabi
    libcxx
  ];
  libPath = with xorg; lib.makeLibraryPath (buildInputs ++ [ ]);

  buildPhase = ''
    dpkg -x $src .
  '';

  steam-run = (steam.override { extraPkgs = p: buildInputs; }).run;

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
      for i in wps wpp et wpspdf; do
        # patchelf \
        #   --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        #   --force-rpath --set-rpath "${stdenv.cc.cc.lib}/lib64:${libPath}:$(patchelf --print-rpath $prefix/office6/$i)" \
        #   --ignore-missing="${lib.strings.concatStringsSep "" autoPatchelfIgnoreMissingDeps}" \
        #   $prefix/office6/$i
        substituteInPlace $out/bin/$i \
          --replace /opt/kingsoft/wps-office $prefix
      done
      for i in wps wpp et wpspdf; do
      mv $out/bin/$i $out/bin/.$i-orig
      makeWrapper ${steam-run}/bin/steam-run $out/bin/$i \
        --add-flags $out/bin/.$i-orig \
        --argv0 $i
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
