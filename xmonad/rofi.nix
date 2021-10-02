{ config, pkgs, ... }:
let
  theme-cfg = config.theme;
  inherit (config.lib.formats.rasi) mkLiteral;
  theme = config.theme;
  rofi-theme = {
    "*" = {
      active-foreground = mkLiteral "@foreground";
      normal-background = mkLiteral "@background";
      normal-foreground = mkLiteral "@foreground";
      urgent-foreground = mkLiteral "@foreground";

      alternate-active-background = mkLiteral "@background";
      alternate-active-foreground = mkLiteral "@foreground";
      alternate-normal-background = mkLiteral "@background";
      alternate-normal-foreground = mkLiteral "@foreground";
      alternate-urgent-background = mkLiteral "@background";
      alternate-urgent-foreground = mkLiteral "@foreground";

      selected-active-foreground = mkLiteral "@foreground";
      selected-normal-foreground = mkLiteral "@foreground";
      selected-urgent-foreground = mkLiteral "@foreground";

      background-color = mkLiteral "@background";
      border-color = mkLiteral "@background";
      spacing = mkLiteral "2";
    };

    "#window" = {
      background-color = mkLiteral "@background";
      border = mkLiteral "0";
      border-radius = mkLiteral "0px";
      padding = mkLiteral "2.5ch";
    };

    "#mainbox" = {
      border = mkLiteral "0";
      padding = mkLiteral "0";
    };

    "#message" = {
      border = mkLiteral "0px 0px 0px";
      border-color = mkLiteral "@border-color";
      padding = mkLiteral "1px";
    };

    "#textbox" = { text-color = mkLiteral "@foreground"; };

    "#inputbar" = {
      children = map mkLiteral [
        "prompt"
        "textbox-prompt-colon"
        "entry"
        "case-indicator"
      ];
    };

    "#textbox-prompt-colon" = {
      expand = false;
      str = ":";
      margin = mkLiteral "0px 0.3em 0em 0em";
      text-color = mkLiteral "@normal-foreground";
    };

    "#listview" = {
      fixed-height = mkLiteral "0";
      border = mkLiteral "2px 0px 0px";
      border-color = mkLiteral "@border-color";
      spacing = mkLiteral "2px";
      scrollbar = mkLiteral "true";
      padding = mkLiteral "2px 0px 0px";
      columns = mkLiteral "2";
    };

    "#element" = {
      border = mkLiteral "0";
      border-radius = mkLiteral "15px";
      padding = mkLiteral "3 0 3 4";
    };

    "#element.normal.normal" = {
      background-color = mkLiteral "@normal-background";
      text-color = mkLiteral "@normal-foreground";
    };

    "#element.normal.urgent" = {
      background-color = mkLiteral "@urgent-background";
      text-color = mkLiteral "@urgent-foreground";
    };

    "#element.normal.active" = {
      background-color = mkLiteral "@active-background";
      text-color = mkLiteral "@active-foreground";
    };

    "#element.selected.normal" = {
      background-color = mkLiteral "@selected-normal-background";
      text-color = mkLiteral "@selected-normal-foreground";
    };

    "#element.selected.urgent" = {
      background-color = mkLiteral "@selected-urgent-background";
      text-color = mkLiteral "@selected-urgent-foreground";
    };

    "#element.selected.active" = {
      background-color = mkLiteral "@selected-active-background";
      text-color = mkLiteral "@selected-active-foreground";
    };

    "#element.alternate.normal" = {
      background-color = mkLiteral "@alternate-normal-background";
      text-color = mkLiteral "@alternate-normal-foreground";
    };

    "#element.alternate.urgent" = {
      background-color = mkLiteral "@alternate-urgent-background";
      text-color = mkLiteral "@alternate-urgent-foreground";
    };

    "#element.alternate.active" = {
      background-color = mkLiteral "@alternate-active-background";
      text-color = mkLiteral "@alternate-active-foreground";
    };

    "#scrollbar" = {
      width = mkLiteral "4px";
      border = mkLiteral "0";
      handle-width = mkLiteral "8px";
      padding = mkLiteral "0";
    };

    "#sidebar" = {
      border = mkLiteral "2px 0px 0px";
      border-color = mkLiteral "@border-color";
    };

    "#button" = { text-color = mkLiteral "@normal-foreground"; };

    "#button.selected" = {
      background-color = mkLiteral "@selected-normal-background";
      text-color = mkLiteral "@selected-normal-foreground";
    };

    "#inputbar" = {
      spacing = mkLiteral "0";
      text-color = mkLiteral "@normal-foreground";
      padding = mkLiteral "1px";
    };

    "#case-indicator" = {
      spacing = mkLiteral "0";
      text-color = mkLiteral "@normal-foreground";
    };

    "#entry" = {
      spacing = mkLiteral "0";
      text-color = mkLiteral "@normal-foreground";
    };

    "#prompt" = {
      spacing = mkLiteral "0";
      text-color = mkLiteral "@normal-foreground";
    };
    "#element-icon" = {
      size = mkLiteral "45px";
      margin = mkLiteral "0 12px 0 0";
      background-color = mkLiteral "inherit";
    };
    "*" = {
      active-background = mkLiteral "${theme.yellow}";
      urgent-background = mkLiteral "${theme.blue}";
      selected-active-background = mkLiteral "${theme.background}";
      selected-normal-background = mkLiteral "${theme.background2}";
      selected-urgent-background = mkLiteral "${theme.pink}";
      background = mkLiteral "${theme.background1}";
      foreground = mkLiteral "${theme.foreground1}";
    };
  };
in {
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
    # theme = rofi-theme;
    # plugins = with pkgs.unstable; [
    # rofi-vpn
    # rofi-mpd
    # rofi-emoji
    # rofi-power-menu
    # rofi-file-browser
    # ];
    extraConfig = {
      # show-icons = true;
      icon-theme = "Tela blue";
      drun-icon-theme = "Tela blue";
    };
  };
  home.file.".config/rofi/apps.css".text = builtins.readFile ./rofi/apps.css + ''
    * {
      al:  ${theme-cfg.background}00;
      bg:  ${theme-cfg.background}ff;
      se: ${theme-cfg.background1}ff;
      fg:  ${theme-cfg.foreground}ff;
      ac:  #42A5F5;
    }
  '';
  home.packages = with pkgs; [ rofi-power-menu ];
}
