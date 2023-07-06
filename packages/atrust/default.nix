{ stdenv
, lib
, dpkg
, glibc
, gcc-unwrapped
, autoPatchelfHook
, glib
, libsForQt5
, at-spi2-atk
, cups
, mesa_drivers
, xorg
, alsaLib
, cairo
, gnome2
, gtk3
, gdk-pixbuf
}:
stdenv.mkDerivation rec{
  name = "aTrust";
  version = "1.0.0";

  src = ./aTrustInstaller_amd64.deb;

  autoPatchelfIgnoreMissingDeps = [ "liblber-2.4.so.2" "libldap_r-2.4.so.2" ];
  # Required for compilation
  nativeBuildInputs = [
    autoPatchelfHook # Automatically setup the loader, and do the magic
    libsForQt5.qt5.wrapQtAppsHook
    dpkg
  ];

  # Required at running time
  buildInputs = [
    glibc
    gcc-unwrapped
    glib
    libsForQt5.qt5.qtbase
    at-spi2-atk
    cups
    libsForQt5.qt5.qtx11extras
    mesa_drivers
    xorg.libXScrnSaver
    xorg.libXrandr
    xorg.libXtst
    alsaLib
    cairo
    gnome2.pango
    gtk3
    gdk-pixbuf
  ];

  unpackCmd = "dpkg -x $src .";
  sourceRoot = ".";

  runtimeLibPath = lib.makeLibraryPath [ cups.lib ];

  # Extract and copy executable in $out/bin
  installPhase = ''
    mkdir -p $out
    cp -r opt $out
    cp -r usr $out
    mkdir -p $out/bin

    wrapProgram "$out/usr/share/sangfor/aTrust/aTrustTray2" \
        "''${gappsWrapperArgs[@]}" \
        "''${qtWrapperArgs[@]}" \
        --suffix LD_LIBRARY_PATH : "$runtimeLibPath"
    wrapProgram "$out/usr/share/sangfor/aTrust/aTrustTray" \
        "''${gappsWrapperArgs[@]}" \
        "''${qtWrapperArgs[@]}" \
        --suffix LD_LIBRARY_PATH : "$runtimeLibPath"
  '';

  meta = with lib; {
    description = "atrust";
    license = licenses.unfree;
    maintainers = with lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}
