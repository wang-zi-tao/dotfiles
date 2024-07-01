{
  stdenv,
  lib,
  fetchurl,
  unzip,
}:
stdenv.mkDerivation rec {
  pname = "iosevka-nerd";
  version = "v3.2.0";

  src = fetchurl {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.0/Iosevka.zip";
    sha256 = "sha256-A3sJgG4qK9DshWuo4fQMbz++cc0VhZGmkHU4mfi5RDY=";
  };

  nativeBuildInputs = [ unzip ];

  dontInstall = true;

  unpackPhase = ''
    mkdir -p $out/share/fonts/truetype
    unzip -d . $src
    cp IosevkaNerdFontMono-*.ttf "$out/share/fonts/truetype/"
    cp IosevkaNerdFont-*.ttf "$out/share/fonts/truetype/"
  '';

  meta = { };
}
