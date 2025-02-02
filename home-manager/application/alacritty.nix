{ pkgs, ... }:
let
  setting = ''
    [colors.primary]
    background = "#1a1b26"
    foreground = "#c0caf5"

    #[colors.cursor]
    #cursor = "#c0caf5"
    #text = "#1a1b26"

    # Normal colors
    [colors.normal]
    black = "#15161e"
    red = "#f7768e"
    green = "#9ece6a"
    yellow = "#e0af68"
    blue = "#7aa2f7"
    magenta = "#bb9af7"
    cyan = "#7dcfff"
    white = "#a9b1d6"

    # Bright colors
    [colors.bright]
    black = "#414868"
    red = "#f7768e"
    green = "#9ece6a"
    yellow = "#e0af68"
    blue = "#7aa2f7"
    magenta = "#bb9af7"
    cyan = "#7dcfff"
    white = "#c0caf5"

    [font]
    size = 10.5

    [font.bold]
    family = "Iosevka Nerd Font"
    style = "Bold"

    [font.bold_italic]
    family = "Iosevka Nerd Font"
    style = "Bold Italic"

    [font.glyph_offset]
    x = 0
    y = 0

    [font.italic]
    family = "Iosevka Nerd Font"
    style = "Italic"

    [font.normal]
    family = "Iosevka Nerd Font"
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
  home.file.".config/alacritty/alacritty.toml".text = setting + '''';
  home.file.".config/alacritty/alacritty-drop.toml".text =
    setting
    + ''

      [terminal.shell]
      program = "${pkgs.writeScript "tmuxinator-s-drop" "tmuxinator s  drop"}"
    '';
}
