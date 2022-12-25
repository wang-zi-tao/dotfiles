{ stdenv, fetchgit, lib, makeWrapper, electron, makeDesktopItem, imagemagick, fetchurl }:

let
  desktopItem = makeDesktopItem {
    name = "icalingua";
    desktopName = "Icalingua";
    comment = "A Linux client for QQ and more";
    icon = "icalingua";
    exec = ''bash -c "icalingua %u > /dev/null"'';
    categories = "Network";
  };
in
stdenv.mkDerivation
rec {
  pname = "icalingua";
  version = "9ba802";
  src = fetchgit {
    url =
      "https://github.com/MayuriNFC/Icalingua/";
    rev = "9ba8027019daf74b28ab3c9edd2b9f164da6c940";
    sha256 = "1j52b65cpzzjx31n19m0xgxfp76rcvvmfvh8q4nn6w1b4rvww067";
  };
  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p "$out/bin"
    makeWrapper "${electron}/bin/electron" "$out/bin/icalingua" \
      --add-flags "$out/share/icalingua/app.asar"
    install -D "$src" "$out/share/icalingua/app.asar"
    install -D "${desktopItem}/share/applications/"* \
      --target-directory="$out/share/applications/"
    icon_dir="$out/share/icons/hicolor"
    ls -lR
    for s in 16 24 32 48 64 128 256 512; do
      size="''${s}x''${s}"
      echo "create icon \"$size\""
      mkdir -p "$icon_dir/$size/apps"
      echo ${imagemagick}/bin/convert -resize "$size" "${src}/512x512.png" "$icon_dir/$size/apps/icalingua.png"
      ${imagemagick}/bin/convert -resize "$size" "${src}/512x512.png" "$icon_dir/$size/apps/icalingua.png"
    done
  '';

  meta = with lib; {
    description = "A Linux client for QQ and more";
    homepage = "https://github.com/Clansty/Icalingua";
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
  };
} "https://github.com/MayuriNFC/icalingua"
