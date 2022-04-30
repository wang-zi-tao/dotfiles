{ config, pkgs, ... }:
let
  theme = config.theme;
  eww = "${pkgs.eww}/bin/eww";
in
{
  home.packages = [ pkgs.eww pkgs.jq pkgs.i3lock pkgs.wmctrl ];
  home.file.".copnfig/betterlockscreenrc".text = ''
    blur_level=1
    #loginbox=${theme.background}
    ringcolor=${theme.sky}
    insidewrongcolor=${theme.sky}
    keyhlcolor=${theme.sky}
    bshlcolor=${theme.sky}
    wrongcolor=${theme.sky}
    modifcolor=${theme.sky}
    bgcolor=${theme.background}
    loginshadow=${theme.background}
    insidecolor=${theme.background}
    separatorcolor=${theme.background}
    insidevercolor=${theme.background}
    timecolor=${theme.foreground}
    ringwrongcolor=${theme.foreground}
    ringvercolor=${theme.foreground}
    ringcolor=${theme.foreground}
    timecolor=${theme.foreground}
    greetercolor=${theme.foreground}
    layoutcolor=${theme.foreground}
    verifcolor=${theme.foreground}
    i3lockcolor_bin=${pkgs.i3lock-color}/bin/i3lock-color
  '';
  home.file.".config/eww/eww.scss".text = builtins.readFile ./eww.scss + ''
    .highlight-workspace {
      color: ${theme.background};
    }
    .todo_bax,
    .hddbox,
    .layout,
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
    .normal-workspace,
    .hdd_label,
    .uphour,
    .bar,
    .upmin{
      color: ${theme.foreground1};
    }
    .bar-music,
    .label_quote{
      color: ${theme.foreground2};
    }
    .normal-workspace:nth-child(6),
    .iconcpu ,
    .color_red ,
    .label_folder1 ,
    .iconfolder1 ,
    .btn_logout ,
    .warn ,
    .label_stat {
      color: ${theme.red};
    }
    .normal-workspace:nth-child(5),
    .icondisk,
    .color_yellow ,
    .label_folder3 ,
    .iconfolder3 ,
    .btn_reboot ,
    .btn_next ,
    .btn_prev ,
    .artist ,
    .brightness_icon,
    .time_day{
      color: ${theme.yellow};
    }
    .normal-workspace:nth-child(10),
    .normal-workspace:nth-child(3),
    .iconmem,
    .color_green,
    .label_folder2 ,
    .iconfolder2 ,
    .btn_sleep ,
    .btn_play ,
    .time_mer {
      color: ${theme.green};
    }
    .normal-workspace:nth-child(9),
    .normal-workspace:nth-child(2),
    .iconbat,
    .layout,
    .color_sky ,
    .label_folder6 ,
    .iconfolder6 ,
    .hddicon ,
    .btn_poweroff ,
    .volume_icon,
    .todo_add ,
    .bar-circular-inner,
    .song {
      color: ${theme.sky};
    }
    .normal-workspace:nth-child(7),
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
    .normal-workspace:nth-child(8),
    .icontimer {
      color: ${theme.purple};
    }
    .normal-workspace:nth-child(8),
    .normal-workspace:nth-child(4) {
      color: ${theme.pink};
    }
    .genwin,
    .bar,
    {
      background-color: ${theme.background};
    }
    .normal-workspace,
    .bar-circular,
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
    .sidestuff scale trough highlight,
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
    .highlight-workspace:nth-child(4){
      background-color: ${theme.pink};
    }
  '';
  home.file.".config/eww/scripts/weather_info.sh" = {
    source = ./scripts/weather_info.sh;
    executable = true;
  };
  home.file.".config/eww/scripts/trigger.sh" = {
    source = ./scripts/trigger.sh;
    executable = true;
  };
  home.file.".config/eww/images/profile.jpg" = {
    source = ../../../static/profile.png;
    executable = true;
  };
  home.file.".config/eww/eww.yuck".text =
    builtins.replaceStrings [ "$$" ] [ "$" ] ''
      (deflisten workspaces :initial "" "${pkgs.xmonad-log}/bin/xmonad-log")

      (defpoll volume :interval "1s" "${pkgs.alsaUtils}/bin/amixer get Master | grep 'Left:' | awk -F'[][]' '{ print $$2 }' | tr -d '%' | head -1 || echo 0")

      (defpoll time :interval "1s" "${pkgs.busybox}/bin/date '+%m月%d日 %H:%M:%S'")
      (defvar IMAGE "file:///home/wangzi/.config/eww/images/profile.jpg")
      (defvar NAME "wangzi")

      (defpoll UNAME :run-while control-center-enable :interval "5m"  "${pkgs.busybox}/bin/whoami")

      (defpoll HOST :run-while control-center-enable :interval "5s"  "${pkgs.busybox}/bin/hostname")

      (defpoll HOUR :run-while control-center-enable :interval "5s"  "${pkgs.busybox}/bin/date +\"%I\"")
      (defpoll SEC :run-while control-center-enable :interval "1s"  "${pkgs.busybox}/bin/date +\"%S\"")
      (defpoll MIN :run-while control-center-enable :interval "5s"  "${pkgs.busybox}/bin/date +\"%M\"")
      (defpoll MER :run-while control-center-enable :interval "5s"  "${pkgs.busybox}/bin/date +\"%p\"")
      (defpoll DAY :run-while control-center-enable :interval "5s"  "${pkgs.busybox}/bin/date +\"%A\"")

      (defpoll UPHOUR :run-while control-center-enable :interval "5s"  "cat /proc/uptime | awk -F. '{printf(\"%d hours\",($$1/3600))}'")
      (defpoll UPMIN :run-while control-center-enable :interval "5s"  "cat /proc/uptime | awk -F. '{printf(\"%d minutes\",($$1 % 3600)/60%60)}'")

      (deflisten SONG :interval ""  "playerctl metadata --format '{{ title }}';playerctl --follow metadata --format '{{ title }}'")
      (deflisten ARTIST :interval ""  "playerctl metadata --format '{{ artist }}';playerctl --follow metadata --format '{{ artist }}'")
      (defpoll STATUS :run-while control-center-enable :interval "1s"  "playerctl status || true")
      (defpoll MUSIC_LENGTH :run-while control-center-enable :interval "5s"  "`playerctl metadata || echo 'length 1 1'` |grep length|tail -n 1|awk '{print $$3}'")
      (defpoll CURRENT :run-while control-center-enable :interval "1s"  "playerctl position || echo 0")
      (defpoll COVER :run-while control-center-enable :interval "1s"  "")
      (defpoll TTIME :run-while control-center-enable :interval "1s"  "")

      (defpoll ICON :run-while control-center-enable :interval "15m"  "~/.config/eww/scripts/weather_info.sh --icon")
      (defpoll STAT :run-while control-center-enable :interval "15m"  "~/.config/eww/scripts/weather_info.sh --stat")
      (defpoll TEMP :run-while control-center-enable :interval "15m"  "~/.config/eww/scripts/weather_info.sh --temp")
      (defpoll WEATHER :run-while control-center-enable :interval "15m"  "~/.config/eww/scripts/weather_info.sh --getdata")
      (defpoll HEX :run-while control-center-enable :interval "15m"  "~/.config/eww/scripts/weather_info.sh --hex")
      (defpoll QUOTE :run-while control-center-enable :interval "15m"  "~/.config/eww/scripts/weather_info.sh --quote")
      (defpoll QUOTE2 :run-while control-center-enable :interval "15m"  "~/.config/eww/scripts/weather_info.sh --quote2")

      (defpoll MAILS :run-while control-center-enable :interval "5m"  "echo 0")

      (defpoll FREE :run-while control-center-enable :interval "5s"  "df -h / | awk '{print $$4}' | tail -n 1 | sed 's/G/GB/'")

      (defwidget bg [] (box :class "bg"))

      (defwidget user [] 
        (box :class "genwin user" :orientation "v" :spacing 35 :space-evenly "false" :vexpand "false" :hexpand "false" 
          (box :class "face" :halign "center")
          (label :class "fullname" :halign "center" :wrap "true" :limit-width 25 :text UNAME)))

      (defwidget system [] 
        (box :class "genwin system" :vexpand "false" :hexpand "false" 
          (box :orientation "v" :spacing 35 :halign "center" :valign "center" :space-evenly "false" :vexpand "false" :hexpand "false" 
            (box :class "cpu_bar" :orientation "h" :spacing 20 :space-evenly "false" :vexpand "false" :hexpand "false" 
              (label :class "iconcpu" :text "")
              (scale :min 0 :max 100 :value {EWW_CPU["avg"]} :active "false"))
            (box :class "mem_bar" :orientation "h" :spacing 20 :space-evenly "false" :vexpand "false" :hexpand "false" 
              (label :class "iconmem" :text "ﲮ")
              (scale :min 0 :max 100 :value {EWW_RAM.used_mem_perc} :active "false"))
            (box :class "disk_bar" :orientation "h" :spacing 20 :space-evenly "false" :vexpand "false" :hexpand "false" 
              (label :class "icondisk" :text "")
              (scale :min 0 :max 100 :value {EWW_DISK["/"].used_perc} :active "false"))
            (box :class "bat_bar" :orientation "h" :spacing 20 :space-evenly "false" :vexpand "false" :hexpand "false" 
              (label :class "iconbat" :text "")
              (scale :min 0 :max 100 :value {EWW_BATTERY.BAT0.capacity} :active "false")))))

      (defwidget clock [] 
        (box :class "genwin clock" :orientation "h" :space-evenly "false" :vexpand "false" :hexpand "false" 
          (label :class "time_hour" :valign "start" :wrap "true" :limit-width 25 :text HOUR)
          (label :class "time_min" :valign "end" :wrap "true" :limit-width 25 :text MIN)
          (box :orientation "v" :spacing 25 :space-evenly "false" :vexpand "false" :hexpand "false" 
            (label :class "time_mer" :halign "end" :wrap "true" :limit-width 25 :text MER)
            (label :class "time_day" :halign "end" :wrap "true" :limit-width 25 :text DAY))))

      (defwidget uptime [] 
        (box :class "genwin uptime" 
          (box :orientation "h" :halign "center" :spacing 40 :space-evenly "false" :vexpand "false" :hexpand "false" 
            (label :class "icontimer" :valign "center" :text "祥")
            (box :orientation "v" :valign "center" :spacing 0 :space-evenly "false" :vexpand "false" :hexpand "false" 
              (label :class "uphour" :halign "start" :wrap "true" :limit-width 25 :text UPHOUR)
              (label :class "upmin" :halign "start" :wrap "true" :limit-width 25 :text UPMIN)))))

      (defwidget music [] 
        (box :class "genwin music" :orientation "v" :space-evenly "false" :vexpand "false" :hexpand "false" 
          (box :orientation "v" :spacing 20 :space-evenly "false" :vexpand "false" :hexpand "false" 
            (label :halign "center" :class "song" :wrap "true" :limit-width 15 :text SONG)
            (label :halign "center" :class "artist" :wrap "true" :limit-width 15 :text ARTIST)
            (box :orientation "h" :spacing 20 :halign "center" :space-evenly "true" :vexpand "false" :hexpand "false" 
              (button :class "btn_prev" :onclick "playerctl previous" "玲")
              (button :class "btn_play" :onclick "playerctl play-pause" {STATUS != "Playing" ? "" : "" })
              (button :class "btn_next" :onclick "playerctl next" "怜"))
            (box :class "music_bar" :halign "center" :vexpand "false" :hexpand "false" 
              (scale :onscroll "playerctl position 1 +" :min 0 :active "true" :max {MUSIC_LENGTH/1000000} :value CURRENT)))))

      (defwidget web1 [] 
        (box :class "web1" :vexpand "false" :hexpand "false" 
          (button :class "iconweb" :onclick "eww set control-center-enable=false;firefox --new-tab https://github.com" "")))

      (defwidget web2 [] 
        (box :class "web2" :vexpand "false" :hexpand "false" 
          (button :class "iconweb" :onclick "~/.config/eww/scripts/open_links.sh --rd" "樓")))

      (defwidget web3 [] 
        (box :class "web3" :vexpand "false" :hexpand "false" 
          (button :class "iconweb" :onclick "eww set control-center-enable=false;firefox --new-tab http://139.9.235.87:30003/zh-cn/" "ﲳ")))

      (defwidget web4 [] 
        (box :class "web4" :vexpand "false" :hexpand "false" 
          (button :class "iconweb" :onclick "eww set control-center-enable=false;firefox --new-tab https://search.nixos.org/packages" "")))

      (defwidget mail [] 
        (box :class "mail" 
          (box :orientation "h" :halign "center" :spacing 20 :space-evenly "false" :vexpand "false" :hexpand "false" 
            (button :class "iconmail" :onclick "~/.config/eww/scripts/open_links.sh --mail" "")
            (box :class "mailbox" :space-evenly "false" :vexpand "false" :hexpand "false" 
              (button :class "label_mails" :onclick "~/.config/eww/scripts/open_links.sh --mail" MAILS)))))

      (defwidget weather [] 
        (box :class "genwin weather"    
          (box :orientation "v" :spacing 10 :space-evenly "false" :vexpand "false" :hexpand "false" 
            (box :orientation "h" :vexpand "false" :hexpand "false" 
              (label :class "iconweather" :halign "start" :style "color: $${HEX};" :text ICON)
              (label :class "label_temp" :halign "end" :text "$${WEATHER != "" ? "" : "" }$${TEMP - 273}°C" ))
            (box :orientation "v" :spacing 10 :halign "center" :space-evenly "false" :vexpand "false" :hexpand "false" 
              (label :class "label_stat" :text STAT)
              (label :class "label_quote" :text QUOTE)
              (label :class "label_quote" :text QUOTE2)))))

      (defwidget apps [] 
        (box :class "genwin" :orientation "v" :space-evenly "false" :vexpand "false" :hexpand "false" 
          (box :class "appbox" :orientation "h" :space-evenly "false" :vexpand "false" :hexpand "false" 
            (button :class "app_fox" :onclick "~/.config/eww/scripts/open_apps.sh --ff/")
            (button :class "app_telegram" :onclick "~/.config/eww/scripts/open_apps.sh --tg")
            (button :class "app_discord" :onclick "~/.config/eww/scripts/open_apps.sh --dc"))
            (box :class "appbox" :orientation "h" :space-evenly "false" :vexpand "false" :hexpand "false" 
              (button :class "app_terminal" :onclick "~/.config/eww/scripts/open_apps.sh --tr")
              (button :class "app_files" :onclick "~/.config/eww/scripts/open_apps.sh --fm")
              (button :class "app_geany" :onclick "~/.config/eww/scripts/open_apps.sh --ge"))
            (box :class "appbox" :orientation "h" :space-evenly "false" :vexpand "false" :hexpand "false" 
              (button :class "app_code" :onclick "~/.config/eww/scripts/open_apps.sh --cd")
              (button :class "app_gimp" :onclick "~/.config/eww/scripts/open_apps.sh --gp")
              (button :class "app_vbox" :onclick "~/.config/eww/scripts/open_apps --vb"))))

      (defwidget logout [] 
        (box :class "genwin" :vexpand "false" :hexpand "false" 
          (button :class "btn_logout" :onclick "betterlockscreen -l -s" "﫼")))
      (defwidget sleep [] 
        (box :class "genwin" :vexpand "false" :hexpand "false" 
          (button :class "btn_sleep" :onclick "eww set control-center-enable=false; betterlockscreen -l -s" "⏾")))
      (defwidget reboot [] 
        (box :class "genwin" :vexpand "false" :hexpand "false" 
          (button :class "btn_reboot" :onclick "systemctl reboot" "")))
      (defwidget poweroff [] 
        (box :class "genwin" :vexpand "false" :hexpand "false" 
          (button :class "btn_poweroff" :onclick "systemctl poweroff" "⏻")))

      (defwidget folder [ icon path index ] 
        (box :orientation "h" :space-evenly "false" :vexpand "false" :hexpand "false" 
          (button :class "iconfolder$${index}" :onclick "~/.config/eww/scripts/open_folders.sh $${path}" icon)
          (button :class "label_folder$${index}" :onclick "~/.config/eww/scripts/open_folders.sh $${path}" path)))

      (defwidget folders [] 
        (box :class "genwin folders" :orientation "v" :space-evenly "false" :vexpand "false" :hexpand "false"      (box :class "hddbox" :orientation "h" :space-evenly "false" :vexpand "false" :hexpand "false" 
          (box :space-evenly "false" :vexpand "false" :hexpand "false"
            (button :class "hddicon" :onclick "~/.config/eww/scripts/open_apps.sh --fm" ""))
          (label :class "fs_sep" :text "|")
          (box :space-evenly "false" :vexpand "false" :hexpand "false" 
            (label :class "hdd_label" :wrap "true" :limit-width 25 :text FREE)))
            (folder :path "工作空间" :icon "" :index 1)
            (folder :path "文档" :icon "" :index 2)
            (folder :path "仓库" :icon "" :index 3)
            (folder :path "下载" :icon "" :index 4)
            (folder :path "图片" :icon "" :index 5)
            (folder :path "Home" :icon "" :index 6)))

      (defpoll TODO :run-while control-center-enable :interval "5s"  "todo --database ~/.todo --mono --use-format display=eww | tail -n 5")
      (defwidget todo [] 
        (box :class "genwin todo" :orientation "v" :space-evenly "false" :spacing 10 :vexpand "false" :hexpand "false" 
         (centerbox :class "todo_box" :orientation "v"
          (box :orientation "h" :class "todo_add_panel" :vexpand "false" :hexpand true :spacing 10 :space-evenly false
            (input :value "add todos" :onchange "echo" :hexpand true :class "todo_input" )
            (button :onclick "echo" :class "todo_add" "" )
          )
          (box)
          (literal :content "(box :spacing 10 :halign \"start\" :orientation \"v\" $${TODO})"))))
      (defwindow todo :stacking "fg" :focusable "true" :screen 1 :geometry (geometry :x 880 :y 490 :width 550 :height 280) (todo))
      (defwindow mail :stacking "fg" :focusable "false" :screen 1 :geometry (geometry :x 1140 :y 785 :width 290 :height 145) (mail))

      (defwindow logout :stacking "fg" :focusable "false" :screen 1 :geometry (geometry :x 1445 :y 150 :width 155 :height 155) (logout))

      (defwindow sleep :stacking "fg" :focusable "false" :screen 1 :geometry (geometry :x 1615 :y 150 :width 155 :height 155) (sleep))

      (defwindow reboot :stacking "fg" :focusable "false" :screen 1 :geometry (geometry :x 1445 :y 320 :width 155 :height 155) (reboot))

      (defwindow poweroff :stacking "fg" :focusable "false" :screen 1 :geometry (geometry :x 1615 :y 320 :width 155 :height 155) (poweroff))

      (defwindow folders :stacking "fg" :focusable "false" :screen 1 :geometry (geometry :x 1445 :y 490 :width 325 :height 440) (folders))

      (defvar control-center-enable false)
      (defwindow control-center
        :monitor 0
        :windowtype "dock"
        :wm-ignore true
        :stacking "fg"
        :focusable control-center-enable
        :screen 1 
        :geometry (geometry :x "0%"
                            :y "0%"
                            :anchor "center center")
        (revealer :transition "slidedown" :duration "1s" :reveal control-center-enable
         (box :orientation "h" :spacing 15 :space-evenly false
          (box :orientation "v" :spacing 15 :space-evenly false
           (user)
           (system))
          (box :orientation "v" :spacing 15 :space-evenly false
           (box :orientation "h" :spacing 15 :space-evenly false
            (box :orientation "v" :spacing 15 :space-evenly false
             (clock)
             (uptime))
            (weather))
           (box :orientation "h" :spacing 15 :space-evenly false
            (todo)
            (music))
           (box :orientation "h" :spacing 15 :space-evenly false
            (box :orientation "h" :spacing 15 :space-evenly false
             (web1)
             (web2)
             (web3)
             (web4))
            (mail)))
           (box :orientation "v" :spacing 15 :space-evenly false
            (box :orientation "h" :spacing 15 
             (logout)
             (sleep))
            (box :orientation "h" :spacing 15 
             (reboot)
             (poweroff))
            (folders)))))

      (defvar volume_expand false)
      (defpoll volume_value :interval "1s" "${pkgs.alsaUtils}/bin/amixer get Master | grep 'Left:' | awk -F'[][]' '{ print $$2 }' | tr -d '%' | head -1 || echo 0")
      (defvar brightness_expand false)
      (defpoll brightness_value :interval "5s" "${pkgs.brightnessctl}/bin/brightnessctl -m -d intel_backlight | awk -F, '{print substr($4, 0, length($4)-1)}' | tr -d '%'")
      (defvar wifi_expand false)
      (defpoll wifi_value :interval "5s" "${pkgs.networkmanager}/bin/nmcli c | grep -E 'wifi' | awk '{print ($1)}' | head -n 1")
      (defwidget layout [text]
        (box :class "workspace layout" :vexpand false
          (button :onclick "rofi -combi-modi window,drun -show combi -modi combi -theme ~/.config/rofi/apps.css&"
            (label :hexpand true :text text))))
      (defwidget ws [type text name]
        (button :class "workspace $${type}" :onclick "${pkgs.wmctrl}/bin/wmctrl -s $$(( $${name} - 1 ))" 
          (button :onclick "rofi -combi-modi window,drun -show combi -modi combi -theme ~/.config/rofi/apps.css&"
             (label :hexpand true :text text))))
      (defwindow bar :monitor 0 :windowtype "dock" :wm-ignore true :stacking "bg" 
        :geometry (geometry :x "0%" :y "0%" :width "100%" :height "0px" :anchor "top center")
        :reserve (struts :side "top" :distance "34px") 
        (centerbox :class "bar" :orientation "h"
          (literal :valign "center" :content "(box :class \"workspaces\" :space-evenly false $${workspaces})")
          (box :class "time" time)
          (box 
            (box
              (box :halign "end" :space-evenly false :spacing 4 
                (box :class "bar-music" :orientation "h" :space-evenly false :valign "center" 
                  (label :limit-width 32 :text "$${SONG} $${ARTIST}")) 
                (circular-progress :value {EWW_CPU["avg"]} :class "color_yellow bar-circular" :thickness 5 
                  (label :class "bar-circular-inner" :text ""))
                (circular-progress :value {EWW_RAM.used_mem_perc} :class "color_green bar-circular" :thickness 5 
                  (label :class "bar-circular-inner" :text "ﲮ"))
                (circular-progress :value {EWW_TEMPS.PACKAGE_ID_0} :class "color_red bar-circular" :thickness 5 
                  (label :class "bar-circular-inner" :text "糖"))
                (circular-progress :value {EWW_BATTERY.BAT0.capacity} :class "color_blue bar-circular" :thickness 5 
                  (label :class "bar-circular-inner" :text "⏻"))
                (eventbox :onhover "${eww} update volume_expand=true" :onhoverlost "${eww} update volume_expand=false"
                  (box :orientation "h" :class "sidestuff" :space-evenly false
                    (label :class "volume_icon" :text "")
                    (revealer :transition "slideleft" :reveal volume_expand :duration "350ms"
                      (scale :min 0 :max 101 :class "bar-volume-scale" :value volume_value :onchange  "${pkgs.alsaUtils}/bin/amixer set Master {}%"))))
                (eventbox :onhover "${eww} update brightness_expand=true" :onhoverlost "${eww} update brightness_expand=false"
                  (box :orientation "h" :class "sidestuff" :space-evenly false
                    (label :class "brightness_icon" :text "")
                    (revealer :transition "slideleft" :reveal brightness_expand :duration "350ms"
                      (scale :min 0 :max 101 :class "bar-brightness-scale" :value brightness_value :onchange  "${pkgs.brightnessctl}/bin/brightnessctl set {}%"))))
                (eventbox :onhover "${eww} update wifi_expand=true" :onhoverlost "${eww} update wifi_expand=false"
                  (box :orientation "h" :class "sidestuff" :space-evenly false
                    (label :class "wifi_icon" :text {wifi_value == "" ? "睊" : "直" })
                    (revealer :transition "slideleft" :reveal wifi_expand :duration "350ms"
                      (label :class "wifi_name" :text wifi_value))))
                (label :class "warn" :text "$${EWW_BATTERY.BAT0.capacity < 20 ? "$${EWW_BATTERY.BAT0.capacity}%" : ""}")
                (label :class "warn" :text "$${EWW_TEMPS.PACKAGE_ID_0 > 75 ? "$${EWW_TEMPS.PACKAGE_ID_0}糖" : ""}")
                (button :onclick "~/.config/eww/scripts/trigger.sh"
                  (box :halign "end" :space-evenly false
                  ;;(box :class "bar-system" :orientation "v" :space-evenly true
                  ;;  (box :class "color_red"
                  ;;    (scale :min 0 :max 100 :class "bar-scale" :value {EWW_CPU["avg"]} :active "false"))
                  ;;  (box :class "color_yellow"
                  ;;    (scale :min 0 :max 100 :class "bar-scale" :value {EWW_RAM.used_mem_perc} :active "false"))
                  ;;  (box :class "color_green"
                  ;;    (scale :min 0 :max 100 :class "bar-scale" :value {EWW_TEMPS.PACKAGE_ID_0} :active "false"))
                  ;;  (box :class "color_sky" 
                  ;;   (scale :min 0 :max 100 :class "bar-scale" :value {EWW_BATTERY.BAT0.capacity} :active "false")))
                    (box :valign "center"
                      (label :class "network_speed" :text {"$${ EWW_NET.wg1.NET_DOWN > 3000000 ? "$${round(EWW_NET.wg1.NET_DOWN/3000000,1)}M" : "$${round(EWW_NET.wg1.NET_DOWN/3000,1)}K"} $${ EWW_NET.wg1.NET_UP > 333333 ? "$${round(EWW_NET.wg1.NET_UP/333333,1)}M" : "$${round(EWW_NET.wg1.NET_UP/333,1)}K"}"})))))))))
    '';
}
