{ pkgs, ... }:
let
  setting = ''
    colors:
      normal:
        black:   '#1e2127'
        red:     '#e06c75'
        green:   '#98c379'
        yellow:  '#d19a66'
        blue:    '#61afef'
        magenta: '#c678dd'
        cyan:    '#56b6c2'
        white:   '#828791'

      # Bright colors
      bright:
        black:   '#5c6370'
        red:     '#e06c75'
        green:   '#98c379'
        yellow:  '#d19a66'
        blue:    '#61afef'
        magenta: '#c678dd'
        cyan:    '#56b6c2'
        white:   '#e6efff'

      # Dim colors
      dim:
        black:   '#1e2127'
        red:     '#e06c75'
        green:   '#98c379'
        yellow:  '#d19a66'
        blue:    '#61afef'
        magenta: '#c678dd'
        cyan:    '#56b6c2'
        white:   '#828791'
      primary:
        background: '#000000'
        foreground: '#FFFFFF'
    font:
      glyph_offset:
        x: 0
        y: 0
      offset:
        x: 0
        y: 0
      size: 10.5
      # use_thin_strokes: true
      normal:
        family: Iosevka Term
        style: Medium
      bold:
        family: Iosevka Term
        style: Bold
      italic:
        family: Iosevka Term
        style: Italic
      bold_italic:
        family: Iosevka Term
        style: Bold Italic
  '';
in
{
  programs.alacritty = {
    enable = true;
  };
  home.file.".config/alacritty/alacritty.yml".text = setting + ''
    window:
      opacity: 0.16
  '';
  home.file.".config/alacritty/alacritty-drop.yml".text = setting + ''
    window:
      opacity: 0.64
    shell:
      program: "${pkgs.writeScript "tmuxinator-s-drop" "tmuxinator s  drop"}"
  '';
}
