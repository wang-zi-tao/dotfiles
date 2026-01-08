pkgs: prev: {
  webssh = prev.webssh.overrideAttrs (oldAttrs: {
    postInstall = ''
      cp "${pkgs.nerd-fonts.iosevka}/share/fonts/truetype/IosevkaNerdFont-Regular.ttf" $out/lib/python3.12/site-packages/webssh/static/css/fonts/
    '';
  });
}
