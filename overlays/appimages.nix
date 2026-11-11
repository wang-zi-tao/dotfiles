pkgs: prev: {

  qq = pkgs.appimageTools.wrapType2 rec {
    pname = "qq";
    version = "3.2.34_260924";
    src = pkgs.fetchurl {
      url = "https://qqdl.gtimg.cn/qqfile/QQNTV2/9.9.36/release/9ee04bef/QQ_3.2.34_260924_x86_64_01.AppImage?sign=523e458bcb546a3e24ddba7e120e08f7&t=1793531085";
      sha256 = "";
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

  wechat = pkgs.appimageTools.wrapType2 rec {
    pname = "wechat";
    version = "1.0.0";
    src = pkgs.fetchurl {
      url = "https://dldir1v6.qq.com/weixin/Universal/Linux/WeChatLinux_x86_64.AppImage";
      sha256 = "";
    };
    extraPkgs = pkgs: with pkgs; [ ];
    extraInstallCommands = ''
      cd $out
      chmod -R +w $out

      # fixup and install desktop file
      mkdir -p $out/share/applications/ 
      echo "
        [Desktop Entry]
        Name=WeChat
        Exec=$out/bin/wechat
        Terminal=false
        Type=Application
        Name[zh_CN]=微信
      " >> $out/share/applications/wechat.desktop
    '';
  };
}
