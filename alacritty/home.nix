{ pkgs, ... }:
let
  setting = ''
    colors:
      bright:
        black: '0x198388'
        blue: '0x277FFF'
        cyan: '0x05A1F7'
        green: '0x47D4B9'
        magenta: '0x962AC3'
        red: '0xEC0101'
        white: '0xFFFFFF'
        yellow: '0xFF8A18'
      normal:
        black: '0x1F2229'
        blue: '0x367BF0'
        cyan: '0x49AEE6'
        green: '0x5EBDAB'
        magenta: '0x9755B3'
        red: '0xD41919'
        white: '0xE6E6E6'
        yellow: '0xFEA44C'
      primary:
        background: '0x000000'
        foreground: '0xFFFFFF'
    font:
      glyph_offset:
        x: 0
        y: 0
      offset:
        x: 0
        y: 0
      size: 11
      use_thin_strokes: true
    shell:
      program: "${pkgs.tmux}/bin/tmux"
  '';
in {
  programs.alacritty = { enable = true; };
  home.file.".config/alacritty/alacritty.yml".text = setting + ''
    background_opacity: 0.45
  '';
  home.file.".config/alacritty/alacritty-drop.yml".text = setting + ''
    background_opacity: 0.75
  '';
}
