pkgs: prev: {
  webssh = prev.webssh.overrideAttrs (oldAttrs: {
    postInstall = ''
      cp "${pkgs.iosevka-nerd}/share/fonts/truetype/IosevkaNerdFont-Regular.ttf" $out/lib/python3.11/site-packages/webssh/static/css/fonts/
    '';
  });
}
