{ pkgs, lib, stdenv, fetchurl, p7zip, dpkg, xwininfo, wqy_microhei, alsa-lib
, alsa-plugins, libpulseaudio, openal, mpg123, gnutls }:
stdenv.mkDerivation rec {
  pname = "deepin-wine-tim";
  version = "3.3.8.22043";

  mirror = "https://com-store-packages.uniontech.com";
  debpkgname = "com.qq.im.deepin";
  debpkgver = "9.3.2deepin20";
  timpkgname = "com.qq.office.deepin";
  src = fetchurl {
    url =
      "${mirror}/appstore/pool/appstore/c/${debpkgname}/${debpkgname}_${debpkgver}_i386.deb";
    sha256 = "sha256-1f+Ke8s/ON9nQPYKTW8FgpwHHuKEiPlSVVPXFgvPLMI=";
  };
  unpackCmd = "dpkg -x $src .";
  sourceRoot = ".";
  exe = fetchurl {
    url = "https://dldir1.qq.com/qqfile/qq/PCTIM/TIM3.3.8/TIM${version}.exe";
    sha256 = "sha256-Q1sRuae+lNt6JJYDnwHXm5uciVlo5uvCAzQKPj0tjl0=";
  };
  nativeBuildInputs = [ p7zip dpkg ];
  buildInputs = [
    xwininfo
    wqy_microhei
    alsa-lib
    alsa-plugins
    libpulseaudio
    openal
    mpg123
    gnutls
  ];

  buildPhase = ''
    ls -l
    echo "Extracting DPKG package ..."
    mkdir -p "dpkgdir"
    # tar -xvf data.tar.xz -C "dpkgdir"
    #sed "s/\(Categories.*$\)/\1Network;/" -i "dpkgdir/usr/share/applications/deepin.com.qq.office.desktop"
    #sed "13s/TIM.exe/tim.exe/" -i "dpkgdir/usr/share/applications/deepin.com.qq.office.desktop"
    echo "Extracting Deepin Wine QQ archive ..."
    7z x -aoa "dpkgdir/opt/apps/${debpkgname}/files/files.7z" -o"deepintimdir"
    echo "Cleaning up the original package directory ..."
    rm -r "deepintimdir/drive_c/Program Files/Tencent/QQ"
    #echo "Patching reg files ..."
    #patch -p1 -d "deepintimdir/" < "reg.patch"
    echo "Creating font file link ..."
    ln -sf "/usr/share/fonts/wenquanyi/wqy-microhei/wqy-microhei.ttc" "deepintimdir/drive_c/windows/Fonts/wqy-microhei.ttc"
    echo "Copying latest TIM installer to deepintimdir/drive_c/Program Files/Tencent/ ..."
    install -m644 "TIM${version}.exe" "deepintimdir/drive_c/Program Files/Tencent/"
    echo "Repackaging app archive ..."
    7z a -t7z -r "files.7z" "deepintimdir/*" 
  '';

  meta = with lib; {
    description = "Tencent TIM on Deepin Wine5(${timpkgname}) For Archlinux";
    homepage = "http://aur.archlinux.org/packages/deepin-wine-tim";
    platforms = [ "x86_64-linux" ];
  };
}
