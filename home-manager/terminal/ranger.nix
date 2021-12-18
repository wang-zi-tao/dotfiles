{ config, pkgs, lib, ... }: {
  home.packages = with pkgs; [
    unstable.ranger
    unrar
    unzip
    odt2txt
    catdoc
    catdocx
  ];
}
