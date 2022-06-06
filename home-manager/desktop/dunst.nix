{ config, pkgs, ... }:

let theme = config.theme;
in
{
  services.dunst = {
    enable = false;
    iconTheme = {
      size = "32x32";
      name = "Tela-blue";
      package = pkgs.tela-icon-theme;
    };
    settings = {
      global = {
        browser = "${pkgs.firefox}/bin/firefox -new-tab";
        dmenu = "${pkgs.dmenu}/bin/dmenu";
        follow = "keyboard";
        geometry = "300x5-30+20";
        format = ''
          <b>%s</b>
          %b'';
        alignment = "center";
        font = "Terminus (TTF) 10";
        padding = 8;
        line_height = 4;
        transparency = 10;
        horizontal_padding = 16;
        corner_radius = 12;
        foreground = theme.foreground1;
        background = theme.background;
        frame_color = theme.sky;
      };
      experimental = { per_monitor_dpi = true; };
      urgency_low = {
        foreground = theme.foreground;
        background = theme.background;
        frame_color = theme.sky;
        timeout = 10;
      };
      urgency_normal = {
        foreground = theme.foreground1;
        background = theme.background;
        frame_color = theme.blue;
        timeout = 20;
      };
      urgency_critical = {
        foreground = theme.background;
        background = theme.pink;
        frame_color = theme.background;
        timeout = 0;
      };
    };
  };
}
