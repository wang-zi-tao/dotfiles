pkgs: prev: {
  webssh = prev.webssh.overrideAttrs (oldAttrs: {
    postInstall = ''
      cp "${pkgs.nerdfonts}/share/fonts/truetype/NerdFonts/IosevkaNerdFontMono-Regular.ttf" $out/lib/python3.11/site-packages/webssh/static/css/fonts/
    '';
  });
}
