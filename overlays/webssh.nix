pkgs: prev: {
  webssh = prev.webssh.overrideAttrs (oldAttrs: {
    postInstall = ''
      # cp "${pkgs.nerdfonts}/share/fonts/truetype/NerdFonts/Sauce Code Pro Nerd Font Complete Mono.ttf" $out/lib/python3.9/site-packages/webssh/static/css/fonts/
      cp "${pkgs.nerdfonts}/share/fonts/truetype/NerdFonts/Iosevka Nerd Font Complete Mono.ttf" $out/lib/python3.9/site-packages/webssh/static/css/fonts/
    '';
  });
}
