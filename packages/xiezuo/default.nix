{ lib
, stdenv
, fetchurl
, autoPatchelfHook
, dpkg
, wrapGAppsHook
, libsForQt5
, xorg
, alsa-lib
, cups
, libXdamage
, nspr
, libcxx
, steam
, mesa
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
  noAuditTmpdir = true;
  autoPatchelfIgnoreMissingDeps = [ ];
  buildInputs = with xorg; [
    alsa-lib
    libXdamage
    nspr
    cups.lib
    libcxx
    libxshmfence
    libsForQt5.fcitx-qt5
    mesa
  ];
  # libPath = with xorg;
  #   lib.makeLibraryPath buildInputs;
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
      # patchelf \
      #   --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      #   --force-rpath --set-rpath "${stdenv.cc.cc.lib}/lib64:$(patchelf --print-rpath $prefix/xiezuo)" \
      #   $prefix/xiezuo
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
