{ lib, appimageTools, fetchurl }:
appimageTools.wrapType2 {
  name = "wewechat";
  src = fetchurl {
    url =
      "https://github.com/trazyn/weweChat/releases/download/v1.1.7/wewechat-1.1.7-linux-x86_64.AppImage";
    sha256 = "sha256-bwFkO73j9GPwstImxzBEXTFMyLSQqxkCRGtNdspJHmc=";
  };
  extraPkgs = pkgs: with pkgs; [ ];
}
