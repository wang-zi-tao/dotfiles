pkgs: prev: {

  qq = pkgs.appimageTools.wrapType2 rec{
    name = "qq";
    src = pkgs.fetchurl {
      url =
        "https://dldir1.qq.com/qqfile/qq/QQNT/ad5b5393/linuxqq_3.1.2-13107_x86_64.AppImage";
      sha256 = "sha256-rCwcu6JVUW9PxMpOHrHrgT38DUg0PEcLmS6nkyQtBN4=";
    };
    extraPkgs = pkgs: with pkgs; [ ];
    extraInstallCommands = ''
      cd $out
      chmod -R +w $out

      # fixup and install desktop file
      mkdir -p $out/share/applications/ 
      echo "
        [Desktop Entry]
        Name=QQ
        Exec=$out/bin/qq
        Terminal=false
        Type=Application
      " >> $out/share/applications/qq.desktop
    '';
  };
}
