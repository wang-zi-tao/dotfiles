{ pkgs, ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      shell = { program = "${pkgs.tmux}/bin/tmux"; };
      font = {
        normal = { family = "Source Code Pro Medium"; };
        bold = { family = "Source Code Pro Medium"; };
        size = 11.0;
        offset = {
          x = 0;
          y = 0;
        };
        glyph_offset = {
          x = 0;
          y = 0;
        };
        use_thin_strokes = true;
      };
      background_opacity = 0.64;
      colors = {
        primary = {
          background = "0x000000";
          foreground = "0xFFFFFF";
        };
        # Normal colors
        normal = {
          black = "0x1F2229";
          red = "0xD41919";
          green = "0x5EBDAB";
          yellow = "0xFEA44C";
          blue = "0x367BF0";
          magenta = "0x9755B3";
          cyan = "0x49AEE6";
          white = "0xE6E6E6";
        };
        # Bright colors
        bright = {
          black = "0x198388";
          red = "0xEC0101";
          green = "0x47D4B9";
          yellow = "0xFF8A18";
          blue = "0x277FFF";
          magenta = "0x962AC3";
          cyan = "0x05A1F7";
          white = "0xFFFFFF";
        };
      };
    };
  };
}
