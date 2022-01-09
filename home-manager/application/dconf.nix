# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let mkTuple = lib.hm.gvariant.mkTuple;
in {
  dconf.settings = {
    "apps/light-locker" = {
      idle-hint = false;
      late-locking = true;
      lock-after-screensaver = "uint32 5";
      lock-on-lid = true;
      lock-on-suspend = true;
    };

    "ca/desrt/dconf-editor" = {
      saved-pathbar-path = "/org/gnome/calendar/window-position";
      saved-view = "/org/gnome/calendar/window-position";
      show-warning = false;
      window-height = 500;
      window-is-maximized = false;
      window-width = 540;
    };

    "com/gexperts/Tilix" = { prompt-on-close-process = false; };

    "com/github/amezin/ddterm" = {
      command = "custom-command";
      custom-command = "tmux";
      ddterm-toggle-hotkey = [ "<Super>x" ];
      palette = [
        "rgb(0x2e, 0x34, 0x36)"
        "rgb(0xcc, 0x00, 0x00)"
        "rgb(0x4e, 0x9a, 0x06)"
        "rgb(0xc4, 0xa0, 0x00)"
        "rgb(0x34, 0x65, 0xa4)"
        "rgb(0x75, 0x50, 0x7b)"
        "rgb(0x06, 0x98, 0x9a)"
        "rgb(0xd3, 0xd7, 0xcf)"
        "rgb(0x55, 0x57, 0x53)"
        "rgb(0xef, 0x29, 0x29)"
        "rgb(0x8a, 0xe2, 0x34)"
        "rgb(0xfc, 0xe9, 0x4f)"
        "rgb(0x72, 0x9f, 0xcf)"
        "rgb(0xad, 0x7f, 0xa8)"
        "rgb(0x34, 0xe2, 0xe2)"
        "rgb(0xee, 0xee, 0xec)"
      ];
      panel-icon-type = "none";
      shortcuts-enabled = true;
      tab-policy = "always";
      tab-position = "bottom";
      theme-variant = "dark";
      window-monitor = "primary";
    };

    "com/github/libpinyin/ibus-libpinyin/libpinyin" = {
      display-style = 1;
      lookup-table-page-size = 8;
      network-dictionary-end-timestamp = "int64 1600522231";
      network-dictionary-start-timestamp = "int64 1600522231";
      sort-candidate-option = 0;
    };

    "org/gnome/Weather" = {
      locations =
        "[<(uint32 2, <('重庆市, 重庆市, 中国', '', false, [(0.51600205200540983, 1.8596112594239504)], [(0.51596781213614429, 1.8596968032465784)])>)>]";
    };

    "org/gnome/baobab/ui" = {
      window-size = mkTuple [ 960 600 ];
      window-state = 87168;
    };

    "org/gnome/builder" = {
      window-maximized = true;
      window-position = mkTuple [ 0 26 ];
      window-size = mkTuple [ 1920 1006 ];
    };

    "org/gnome/calendar" = {
      active-view = "month";
      window-maximized = true;
      window-position = mkTuple [ 0 26 ];
      window-size = mkTuple [ 1920 1054 ];
    };

    "org/gnome/control-center" = { last-panel = "info-overview"; };

    "org/gnome/desktop/app-folders" = {
      folder-children = [ "Utilities" "YaST" ];
    };

    "org/gnome/desktop/app-folders/folders/Utilities" = {
      apps = [
        "gnome-abrt.desktop"
        "gnome-system-log.desktop"
        "nm-connection-editor.desktop"
        "org.gnome.baobab.desktop"
        "org.gnome.DejaDup.desktop"
        "org.gnome.Dictionary.desktop"
        "org.gnome.DiskUtility.desktop"
        "org.gnome.eog.desktop"
        "org.gnome.Evince.desktop"
        "org.gnome.FileRoller.desktop"
        "org.gnome.fonts.desktop"
        "org.gnome.seahorse.Application.desktop"
        "org.gnome.tweaks.desktop"
        "org.gnome.Usage.desktop"
        "vinagre.desktop"
      ];
      categories = [ "X-GNOME-Utilities" ];
      name = "X-GNOME-Utilities.directory";
      translate = true;
    };

    "org/gnome/desktop/app-folders/folders/YaST" = {
      categories = [ "X-SuSE-YaST" ];
      name = "suse-yast.directory";
      translate = true;
    };

    "org/gnome/desktop/background" = {
      color-shading-type = "solid";
      picture-options = "zoom";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
    };

    "org/gnome/desktop/calendar" = { show-weekdate = false; };

    "org/gnome/desktop/input-sources" = {
      mru-sources =
        [ (mkTuple [ "ibus" "libpinyin" ]) (mkTuple [ "xkb" "us" ]) ];
      per-window = false;
      sources = [ (mkTuple [ "ibus" "libpinyin" ]) (mkTuple [ "xkb" "us" ]) ];
      xkb-options = [ "lv3:ralt_switch" ];
    };

    "org/gnome/desktop/interface" = {
      clock-show-seconds = true;
      clock-show-weekday = true;
      cursor-size = 24;
      cursor-theme = "Layan-white-cursors";
      document-font-name = "Sans 11";
      enable-animations = true;
      font-antialiasing = "rgba";
      font-hinting = "slight";
      # font-name = "Sans 10";
      gtk-im-module = "ibus";
      gtk-theme = "Orchis-light";
      icon-theme = "Tela-blue";
      locate-pointer = true;
      monospace-font-name = "Source Code Pro 10";
      show-battery-percentage = true;
      toolbar-style = "text";
      toolkit-accessibility = false;
    };

    "org/gnome/desktop/notifications" = {
      application-children = [
        "org-moson-matray"
        "org-gnome-shell-extensions-gsconnect"
        "org-gnome-extensions-desktop"
        "org-manjaro-pamac-manager"
        "gnome-power-panel"
        "pycharm-community-pycharm-community"
        "gnome-network-panel"
        "org-gnome-nautilus"
        "firefox"
        "qv2ray"
        "alacritty"
        "org-gnome-baobab"
      ];
    };

    "org/gnome/desktop/notifications/application/alacritty" = {
      application-id = "Alacritty.desktop";
    };

    "org/gnome/desktop/notifications/application/firefox" = {
      application-id = "firefox.desktop";
    };

    "org/gnome/desktop/notifications/application/gnome-network-panel" = {
      application-id = "gnome-network-panel.desktop";
    };

    "org/gnome/desktop/notifications/application/gnome-power-panel" = {
      application-id = "gnome-power-panel.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-baobab" = {
      application-id = "org.gnome.baobab.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-extensions-desktop" =
      {
        application-id = "org.gnome.Extensions.desktop.desktop";
      };

    "org/gnome/desktop/notifications/application/org-gnome-nautilus" = {
      application-id = "org.gnome.Nautilus.desktop";
    };

    "org/gnome/desktop/notifications/application/org-gnome-shell-extensions-gsconnect" =
      {
        application-id = "org.gnome.Shell.Extensions.GSConnect.desktop";
      };

    "org/gnome/desktop/notifications/application/org-manjaro-pamac-manager" = {
      application-id = "org.manjaro.pamac.manager.desktop";
    };

    "org/gnome/desktop/notifications/application/org-moson-matray" = {
      application-id = "org.moson.matray.desktop";
    };

    "org/gnome/desktop/notifications/application/pycharm-community-pycharm-community" =
      {
        application-id = "pycharm-community_pycharm-community.desktop";
      };

    "org/gnome/desktop/notifications/application/qv2ray" = {
      application-id = "qv2ray.desktop";
    };

    "org/gnome/desktop/peripherals/keyboard" = { numlock-state = true; };

    "org/gnome/desktop/peripherals/mouse" = { speed = 1.0; };

    "org/gnome/desktop/peripherals/touchpad" = {
      speed = 1.0;
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/privacy" = {
      disable-camera = true;
      disable-microphone = true;
      old-files-age = "uint32 7";
    };

    "org/gnome/desktop/screensaver" = {
      color-shading-type = "solid";
      lock-delay = "uint32 0";
      picture-options = "zoom";
      primary-color = "#000000000000";
      secondary-color = "#000000000000";
    };

    "org/gnome/desktop/search-providers" = {
      sort-order = [
        "org.gnome.Contacts.desktop"
        "org.gnome.Documents.desktop"
        "org.gnome.Nautilus.desktop"
      ];
    };

    "org/gnome/desktop/session" = { idle-delay = "uint32 0"; };

    "org/gnome/desktop/wm/keybindings" = { panel-main-menu = [ "<Alt>F1" ]; };

    "org/gnome/desktop/wm/preferences" = {
      button-layout = "close,maximize,minimize:";
    };

    "org/gnome/epiphany/state" = {
      is-maximized = false;
      window-position = mkTuple [ 0 0 ];
      window-size = mkTuple [ 1024 768 ];
    };

    "org/gnome/evolution-data-server" = {
      migrated = true;
      network-monitor-gio-name = "";
    };

    "org/gnome/file-roller/dialogs/extract" = {
      recreate-folders = true;
      skip-newer = false;
    };

    "org/gnome/file-roller/listing" = {
      list-mode = "as-folder";
      name-column-width = 250;
      show-path = false;
      sort-method = "name";
      sort-type = "ascending";
    };

    "org/gnome/file-roller/ui" = {
      sidebar-width = 200;
      window-height = 480;
      window-width = 600;
    };

    "org/gnome/gedit/plugins" = {
      active-plugins = [ "spell" "sort" "modelines" "filebrowser" "docinfo" ];
    };

    "org/gnome/gedit/plugins/filebrowser" = {
      root = "file:///";
      tree-view = true;
      virtual-root = "file:///home/wangzi/.cache/.fr-NahKcE";
    };

    "org/gnome/gedit/preferences/editor" = {
      background-pattern = "grid";
      insert-spaces = false;
      wrap-last-split-mode = "word";
    };

    "org/gnome/gedit/preferences/ui" = { show-tabs-mode = "auto"; };

    "org/gnome/gedit/state/window" = {
      bottom-panel-size = 140;
      side-panel-active-page = "GeditWindowDocumentsPanel";
      side-panel-size = 200;
      size = mkTuple [ 900 700 ];
      state = 87040;
    };

    "org/gnome/gnome-system-monitor" = {
      cpu-stacked-area-chart = true;
      current-tab = "disks";
      maximized = false;
      network-total-in-bits = false;
      show-dependencies = false;
      show-whose-processes = "all";
      window-state = mkTuple [ 1440 560 ];
    };

    "org/gnome/gnome-system-monitor/disktreenew" = {
      col-6-visible = true;
      col-6-width = 0;
    };

    "org/gnome/gnome-system-monitor/proctree" = {
      columns-order =
        [ 0 1 2 3 4 6 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 ];
      sort-col = 8;
      sort-order = 0;
    };

    "org/gnome/gthumb/browser" = {
      fullscreen-sidebar = "hidden";
      fullscreen-thumbnails-visible = false;
      maximized = false;
      sidebar-sections = [
        "GthFileProperties:expanded"
        "GthFileComment:expanded"
        "GthFileDetails:expanded"
        "GthImageHistogram:expanded"
      ];
      sidebar-visible = true;
      sort-inverse = false;
      sort-type = "file::mtime";
      startup-current-file =
        "'file:///home/wangzi/%E4%B8%8B%E8%BD%BD/2ebf1cdf5aa52273.png'";
      startup-location = "'file:///home/wangzi/%E4%B8%8B%E8%BD%BD'";
      statusbar-visible = true;
      thumbnail-list-visible = true;
      viewer-sidebar = "tools";
    };

    "org/gnome/gthumb/crop" = {
      aspect-ratio = "none";
      aspect-ratio-height = 1;
      aspect-ratio-invert = false;
      aspect-ratio-width = 1;
      bind-dimensions = false;
      bind-factor = 8;
      grid-type = "thirds";
    };

    "org/gnome/gthumb/data-migration" = { catalogs-2-10 = true; };

    "org/gnome/gthumb/general" = {
      active-extensions = [
        "23hq"
        "bookmarks"
        "burn_disc"
        "catalogs"
        "change_date"
        "comments"
        "contact_sheet"
        "convert_format"
        "desktop_background"
        "edit_metadata"
        "exiv2_tools"
        "facebook"
        "file_manager"
        "find_duplicates"
        "flicker"
        "gstreamer_tools"
        "gstreamer_utils"
        "image_print"
        "image_rotation"
        "importer"
        "jpeg_utils"
        "list_tools"
        "oauth"
        "photo_importer"
        "picasaweb"
        "raw_files"
        "red_eye_removal"
        "rename_series"
        "resize_images"
        "search"
        "selections"
        "slideshow"
        "terminal"
        "webalbums"
      ];
    };

    "org/gnome/meld/window-state" = {
      height = 400;
      is-maximized = true;
      width = 893;
    };

    "org/gnome/mutter" = {
      dynamic-workspaces = true;
      overlay-key = "Super_L";
    };

    "org/gnome/nautilus/icon-view" = { default-zoom-level = "small"; };

    "org/gnome/nautilus/list-view" = { use-tree-view = true; };

    "org/gnome/nautilus/preferences" = {
      click-policy = "single";
      default-folder-viewer = "list-view";
      search-filter-time-type = "last_modified";
      show-create-link = true;
    };

    "org/gnome/nautilus/window-state" = {
      initial-size = mkTuple [ 1315 528 ];
      maximized = false;
    };

    "org/gnome/power-manager" = {
      info-history-time = 7200;
      info-history-type = "rate";
      info-last-device = "/org/freedesktop/UPower/devices/battery_BAT0";
      info-page-number = 0;
      info-stats-type = "discharge-accuracy";
    };

    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-schedule-automatic = false;
      night-light-schedule-from = 0.0;
      night-light-temperature = "uint32 4700";
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      area-screenshot = [ ];
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
      ];
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" =
      {
        binding = "<Primary><Alt>t";
        command = "alacritty";
        name = "alacritty";
      };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-timeout = 7200;
      sleep-inactive-ac-type = "nothing";
    };

    "org/gnome/shell" = {
      command-history = [ "r" ];
      disable-extension-version-validation = true;
      disable-user-extensions = false;
      disabled-extensions = [
        "clippie@blackjackshellac.ca"
        "extensions-sync@elhan.io"
        "places-menu@gnome-shell-extensions.gcampax.github.com"
        "dash-to-dock@micxgx.gmail.com"
        "drive-menu@gnome-shell-extensions.gcampax.github.com"
        "kimpanel@kde.org"
        "task-widget@juozasmiskinis.gitlab.io"
      ];
      enabled-extensions = [
        "arcmenu@arcmenu.com"
        "blur-my-shell@aunetx"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "autohide-battery@sitnik.ru"
        "GPaste@gnome-shell-extensions.gnome.org"
        "gtktitlebar@velitasali.github.io"
        "gsconnect@andyholmes.github.io"
        "nightthemeswitcher@romainvigier.fr"
        "pamac-updates@manjaro.org"
        "panelScroll@sun.wxg@gmail.com"
        "ProxySwitcher@flannaghan.com"
        "runcat@kolesnikov.se"
        "middleclickclose@paolo.tranquilli.gmail.com"
        "system-monitor@paradoxxx.zero.gmail.com"
        "appindicatorsupport@rgcjonas.gmail.com"
        "gnome-shell-screenshot@ttll.de"
        "gTile@vibou"
        "x11gestures@joseexposito.github.io"
        "ddterm@amezin.github.com"
        "hotedge@jonathan.jdoda.ca"
        "netspeedsimplified@prateekmedia.extension"
      ];
      favorite-apps = [
        "firefox.desktop"
        "code.desktop"
        "idea-community.desktop"
        "clion.desktop"
        "pycharm-community.desktop"
        "webstorm.desktop"
        "google-chrome.desktop"
        "org.gnome.Nautilus.desktop"
        "virtualbox.desktop"
        "gnome-system-monitor.desktop"
        "com.wps.Office.wps.desktop"
        "com.wps.Office.wpp.desktop"
        "com.wps.Office.et.desktop"
        "com.wps.Office.pdf.desktop"
        "org.gnome.Calendar.desktop"
        "Alacritty.desktop"
      ];
      had-bluetooth-devices-setup = true;
      welcome-dialog-last-shown-version = "40.2";
    };

    "org/gnome/shell/extensions/arcmenu" = {
      arc-menu-placement = "Panel";
      available-placement = [ true false false ];
      distro-icon = 6;
      menu-button-icon = "Distro_Icon";
      pinned-app-list = [
        "Firefox"
        ""
        "firefox.desktop"
        "Terminal"
        ""
        "org.gnome.Terminal.desktop"
        "ArcMenu Settings"
        "ArcMenu_ArcMenuIcon"
        "gnome-extensions prefs arcmenu@arcmenu.com"
      ];
      prefs-visible-page = 0;
      recently-installed-apps = [ "gimp.desktop" ];
    };

    "org/gnome/shell/extensions/blur-my-shell" = {
      blur-dash = true;
      blur-overview = true;
      blur-panel = false;
      brightness = 0.8;
      dash-opacity = 0.12;
      debug = false;
      sigma = 30;
    };

    "org/gnome/shell/extensions/dash-to-dock" = {
      background-opacity = 0.8;
      dash-max-icon-size = 48;
      dock-position = "BOTTOM";
      height-fraction = 0.9;
    };

    "org/gnome/shell/extensions/gsconnect" = {
      devices = [ "636a8ff1bc90495f" "94050a7e76e9fc11" ];
      id = "386952f5-6eac-4fa5-9a82-d09d8b29478f";
      name = "wangzipc";
    };
  };
}
