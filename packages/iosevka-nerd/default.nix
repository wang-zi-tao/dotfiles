{ stdenv
, lib
, fetchurl
, unzip
}:
stdenv.mkDerivation rec {
  pname = "iosevka-nerd";
  version = "15.5.2";

  src = fetchurl {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Iosevka.zip";
    sha256 = "sha256-a9Ke+Ia4CNHXbdhfgriCNFImX9pYKAFzSqtvlGAnDeM=";
  };

  nativeBuildInputs = [ unzip ];

  dontInstall = true;

  unpackPhase = ''
    mkdir -p $out/share/fonts/truetype
    unzip -d . $src
    cp "Iosevka Term Nerd Font Complete.ttf" $out/share/fonts/truetype
    cp "Iosevka Nerd Font Complete.ttf" $out/share/fonts/truetype
    ls $out/share/fonts/truetype/ -lah
    ln -s "$out/share/fonts/truetype/Iosevka Nerd Font Complete Mono.ttf" "$out/share/fonts/truetype/IosevkaNerdFontCompleteMono.ttf"
    ln -s "$out/share/fonts/truetype/Iosevka Nerd Font Complete.ttf" "$out/share/fonts/truetype/IosevkaNerdFontComplete.ttf"
  '';

  meta = { };

}
