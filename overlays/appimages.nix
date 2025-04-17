pkgs: prev: {

  qq = pkgs.appimageTools.wrapType2 rec {
    name = "qq";
    src = pkgs.fetchurl {
      url = "https://dldir1.qq.com/qqfile/qq/QQNT/Linux/QQ_3.2.16_250401_x86_64_01.AppImage";
      sha256 = "sha256-M++enPKnFXYxnHjkkHf/0/2xYVvM1VAG8WZeScnLxuU=";
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
