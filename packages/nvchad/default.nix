{ stdenvNoCC, lib, fetchFromGitHub }:

stdenvNoCC.mkDerivation {
  pname = "NvChad";
  version = "70baf71";

  src = fetchFromGitHub {
    owner = "NvChad";
    repo = "NvChad";
    rev = "70baf718d243b813ab1be931b7ee2d25911040b2";
    sha256 = "sha256-zvPccyYXhIYM2kj7TtfXQqC8kPgxbrS1kyIcDZd2edg=";
  };

  installPhase = ''
    mkdir $out
    cp * -r $out
  '';

  meta = with lib; {
    description = "An attempt to make neovim cli as functional as an IDE while being very beautiful, blazing fast startuptime ~ 0.05 secs";
    homepage = "https://nvchad.github.io/";
    platforms = platforms.unix;
    license = with licenses; [ gpl3 ];
  };
}
