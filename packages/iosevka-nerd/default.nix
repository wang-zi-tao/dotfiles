{
  stdenv,
  lib,
  fetchurl,
  unzip,
}:
stdenv.mkDerivation rec {
  pname = "iosevka-nerd";
  version = "15.5.2";

  src = fetchurl {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.0-RC/Iosevka.zip";
    sha256 = "sha256-PW76QaizFHzZ592uiYoozODHcdFKEDMB6yrQ+UTEcr0=";
  };

  nativeBuildInputs = [ unzip ];

  dontInstall = true;

  unpackPhase = ''
    mkdir -p $out/share/fonts/truetype
    unzip -d . $src
    cp "Iosevka Term Nerd Font Complete.ttf" $out/share/fonts/truetype
    cp "Iosevka Nerd Font Complete.ttf" $out/share/fonts/truetype
    ln -s "$out/share/fonts/truetype/Iosevka Term Nerd Font Complete.ttf" "$out/share/fonts/truetype/IosevkaTermNerdFontComplete.ttf"
    ln -s "$out/share/fonts/truetype/Iosevka Nerd Font Complete.ttf" "$out/share/fonts/truetype/IosevkaNerdFontComplete.ttf"
  '';

  meta = { };
}
