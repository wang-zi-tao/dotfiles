{ pkgs, ... }:
let
  setting = ''
    [colors.bright]
    black = "#5c6370"
    blue = "#61afef"
    cyan = "#56b6c2"
    green = "#98c379"
    magenta = "#c678dd"
    red = "#e06c75"
    white = "#e6efff"
    yellow = "#d19a66"

    [colors.dim]
    black = "#1e2127"
    blue = "#61afef"
    cyan = "#56b6c2"
    green = "#98c379"
    magenta = "#c678dd"
    red = "#e06c75"
    white = "#828791"
    yellow = "#d19a66"

    [colors.normal]
    black = "#1e2127"
    blue = "#61afef"
    cyan = "#56b6c2"
    green = "#98c379"
    magenta = "#c678dd"
    red = "#e06c75"
    white = "#828791"
    yellow = "#d19a66"

    [colors.primary]
    background = "#000000"
    foreground = "#FFFFFF"

    [font]
    size = 10.5

    [font.bold]
    family = "Iosevka Term"
    style = "Bold"

    [font.bold_italic]
    family = "Iosevka Term"
    style = "Bold Italic"

    [font.glyph_offset]
    x = 0
    y = 0

    [font.italic]
    family = "Iosevka Term"
    style = "Italic"

    [font.normal]
    family = "Iosevka Term"
    style = "Medium"

    [font.offset]
    x = 0
    y = 0

    [window]
    opacity = 0.16
  '';
in
{
  programs.alacritty = {
    enable = true;
  };
  home.file.".config/alacritty/alacritty.toml".text = setting + ''
  '';
  home.file.".config/alacritty/alacritty-drop.toml".text = setting + ''

    [shell]
    program = "${pkgs.writeScript "tmuxinator-s-drop" "tmuxinator s  drop"}"
  '';
}
