{
  stdenv,
  lib,
  fetchurl,
  unzip,
}:
stdenv.mkDerivation rec {
  pname = "iosevka-nerd";
  version = "v3.4.0";

  src = fetchurl {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/${version}/Iosevka.zip";
    sha256 = "sha256-Ucfuu4+TPMPRDHGKGRMX5/Aowsfw/H5vXGmWVaeX4C8=";
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
