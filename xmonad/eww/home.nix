{ config, pkgs, ... }:
let theme = config.theme;
in {
  home.packages = [
    (pkgs.callPackage ../../packages/eww/default.nix { })
    pkgs.jq
    pkgs.i3lock
  ];
  home.file.".config/eww/eww.scss".text = builtins.readFile ./eww.scss + ''
    .highlight-workspace {
      color: ${theme.background};
    }
    .todo_bax,
    .hddbox,
    .bar-system scale trough ,
    .sidestuff scale trough ,
    .music_bar scale trough {
      background-color: ${theme.background1};
    }
    .todo_input,
    .cpu_bar scale trough ,
    .mem_bar scale trough ,
    .disk_bar scale trough ,
    .bat_bar scale trough {
      background-color: ${theme.background2};
    }
    .todo_input,
    .todo_item{
      color: ${theme.foreground};
    }
    .hdd_label,
    .uphour,
    .bar,
    .upmin{
      color: ${theme.foreground1};
    }
    .bar-music,
    .volume,
    .label_quote{
      color: ${theme.foreground2};
    }
    .iconcpu ,
    .color_red ,
    .label_folder1 ,
    .iconfolder1 ,
    .btn_logout ,
    .label_stat {
    	color: ${theme.red};
    }
    .icondisk,
    .color_yellow ,
    .label_folder3 ,
    .iconfolder3 ,
    .btn_reboot ,
    .btn_next ,
    .btn_prev ,
    .artist ,
    .time_day{
    	color: ${theme.yellow};
    }
    .iconmem,
    .color_green,
    .label_folder2 ,
    .iconfolder2 ,
    .btn_sleep ,
    .btn_play ,
    .time_mer {
    	color: ${theme.green};
    }
    .layout,
    .iconbat,
    .color_sky ,
    .label_folder6 ,
    .iconfolder6 ,
    .hddicon ,
    .btn_poweroff ,
    .todo_add ,
    .song {
    	color: ${theme.sky};
    }
    .color_blue ,
    .label_folder4 ,
    .iconfolder4 ,
    .time_hour ,
    .time_min {
    	color: ${theme.blue};
    }
    .color_purple ,
    .label_folder5 ,
    .iconfolder5 ,
    .icontimer {
    	color: ${theme.purple};
    }
    .genwin,
    .bar,
    {
      background-color: ${theme.background};
    }
    .normal-workspace,
    {
      background-color: ${theme.background1};
    }
    .highlight-workspace:nth-child(6),
    .cpu_bar scale trough highlight,
    .color_red scale trough highlight{
    	background-color: ${theme.red};
    }
    .highlight-workspace:nth-child(5),
    .disk_bar scale trough highlight ,
    .color_yellow scale trough highlight{
      background-color: ${theme.yellow};
    }
    .highlight-workspace:nth-child(10),
    .highlight-workspace:nth-child(3),
    .mem_bar scale trough highlight ,
    .color_green scale trough highlight{
      background-color: ${theme.green};
    }
    .highlight-workspace:nth-child(9),
    .highlight-workspace:nth-child(2),
    .color_sky scale trough highlight{
      background-color: ${theme.sky};
    }
    .highlight-workspace:nth-child(7),
    .music_bar scale trough highlight ,
    .bat_bar scale trough highlight ,
    .color_blue scale trough highlight{
      background-color: ${theme.blue};
    }
    .highlight-workspace:nth-child(8),
    .color_purple scale trough highlight{
      background-color: ${theme.purple};
    }
    .highlight-workspace:nth-child(8),
    .highlight-workspace:nth-child(4),
    .sidestuff scale trough highlight {
      background-color: ${theme.pink};
    }
        '';
  home.file.".config/eww/scripts/weather_info" = {
    source = ./scripts/weather_info;
    executable = true;
  };
  home.file.".config/eww/scripts/trigger" = {
    source = ./scripts/trigger;
    executable = true;
  };
  home.file.".config/eww/eww.yuck".text = builtins.readFile ./eww.yuck + ''
    (deflisten workspaces :initial ""
      "${pkgs.xmonad-log}/bin/xmonad-log")

    (defpoll volume :interval "1s"
      "${pkgs.alsaUtils}/bin/amixer get Master | grep 'Left:' | awk -F'[][]' '{ print $2 }' | tr -d '%' | head -1")

    (defpoll time :interval "1s"
      "date '+%H:%M:%S %b %d'")
  '';
}
