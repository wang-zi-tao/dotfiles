{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  dpkg,
  wrapGAppsHook,
  libsForQt5,
  xorg,
  alsa-lib,
  atk,
  bzip2,
  cairo,
  cups,
  dbus,
  expat,
  ffmpeg_4,
  rigsofrods-bin,
  mesa,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  gperftools,
  gtk2-x11,
  libICE,
  libpng12,
  libSM,
  libtool,
  libuuid,
  libX11,
  libxcb,
  libXcomposite,
  libXcursor,
  libXdamage,
  libXext,
  libXfixes,
  libXi,
  libxml2,
  libXrandr,
  libXrender,
  libXScrnSaver,
  libXtst,
  nspr,
  nss,
  curl,
  pango,
  sqlite,
  unixODBC,
  xz,
  zlib,
  libcxxabi,
  libcxx,
  nixpkgs-old,
  steam,
}:
stdenv.mkDerivation rec {
  pname = "wpsoffice";
  version = "11.1.0.11711";
  src = fetchurl {
    url = "https://wps-linux-personal.wpscdn.cn/wps/download/ep/Linux2023/17885/wps-office_12.1.0.17885_amd64.deb?t=1726553572&k=756141317e686bfbdcc18572e1f35448";
    sha256 = "sha256-JHSTZZnOZoTpj8zF4C5PmjTkftEdxbeaqweY3ITiJto=";
  };
  unpackCmd = "dpkg -x $src .";
  sourceRoot = ".";

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    wrapGAppsHook
    libsForQt5.qt5.wrapQtAppsHook
  ];

  meta = with lib; {
    description = "Office suite, formerly Kingsoft Office";
    homepage = "https://www.wps.com/";
    platforms = [ "x86_64-linux" ];
    hydraPlatforms = [ ];
    license = licenses.unfreeRedistributable;
    maintainers = with maintainers; [
      mlatus
      th0rgal
    ];
  };

  dontPatchELF = true;

  # wpsoffice uses `/build` in its own build system making nix things there
  # references to nix own build directory
  noAuditTmpdir = true;

  unvendoredLibraries = [
    # Have to use parts of the vendored qt4
    #"Qt"
    "SDL2"
    "bz2"
    "avcodec"
    "avdevice"
    "avformat"
    "avutil"
    "swresample"
    "swscale"
    "jpeg"
    # "png"
    "stdc++"
    # "ssl"
    "curl"
    # "crypto"
    "nspr"
    "nss"
    "odbc"
    "tcmalloc" # gperftools
  ];
  autoPatchelfIgnoreMissingDeps = [ "libkappessframework.so" ];
  buildInputs = with xorg; [
    alsa-lib
    atk
    bzip2
    cairo
    dbus.lib
    expat
    ffmpeg_4
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
    libcxx
  ];
  libPath = with xorg; lib.makeLibraryPath (buildInputs ++ [ ]);

  installPhase =
    let
      steam-run = (steam.override { extraPkgs = p: buildInputs; }).run;
    in
    ''
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
          patchelf \
            --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
            --force-rpath --set-rpath "${stdenv.cc.cc.lib}/lib64:${libPath}:$(patchelf --print-rpath $prefix/office6/$i)" \
            $prefix/office6/$i
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
