{ pkgs, lib, config, ... }:
let
  makeService = service: {
    Unit = {
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
    Service = service;
  };
in
{
  services = {
    blueman-applet.enable = false;
    network-manager-applet.enable = true;
  };
  xsession.windowManager.awesome = {
    enable = true;
  };
  home.file.".config/awesome/resources/".source = "${pkgs.resources}/";
  home.file.".config/awesome/icons" = {
    source = ./icons;
    recursive = true;
  };
  home.file.".config/awesome/widget" = {
    source = ./widget;
    recursive = true;
  };
  home.file.".config/awesome/apps.lua".source = ./apps.lua;
  home.file.".config/awesome/autostart.lua".source = ./autostart.lua;
  home.file.".config/awesome/config.lua".source = ./config.lua;
  home.file.".config/awesome/keys.lua".source = ./keys.lua;
  home.file.".config/awesome/rc.lua".source = ./rc.lua;
  home.file.".config/awesome/react.lua".source = ./react.lua;
  home.file.".config/awesome/rule.lua".source = ./rule.lua;
  home.file.".config/awesome/theme.lua".source = ./theme.lua;
  home.file.".config/awesome/ui.lua".source = ./ui.lua;
  home.file.".config/awesome/signals.lua".source = ./signals.lua;
  home.file.".config/awesome/module/bling" = {
    source = pkgs.fetchgit {
      url = "https://github.com/BlingCorp/bling";
      rev = "e384951c321420dcaada7ad4381a644f2c7eea83";
      sha256 = "sha256-P725jMxPK7TOhLvvwQyyJBSOU48EDQeXIfcuweq6TUw=";
    };
    recursive = true;
  };
  home.file.".config/awesome/module/battery_widget" = {
    source = "${pkgs.fetchgit {
        url = "https://github.com/Aire-One/awesome-battery_widget";
        rev = "48b83f444d175496104f3b9ff36f1dff0473e01e";
        sha256 = "sha256-ELNWKnwHDOxgC30xP3gTT1pYLZgGvGd9eVJKd5Ok98A=";
    }}";
    recursive = true;
  };
  home.file.".config/awesome/module/rubato" = {
    source = pkgs.fetchgit {
      url = "https://github.com/andOrlando/rubato";
      rev = "7ed12e183583a7ce3b59714452217af9a1f02ce6";
      sha256 = "sha256-BixO7PagHUm7pX5V7RTaC3/ffNErGw3bf77qeSLleqI=";
    };
    recursive = true;
  };
  home.file.".config/awesome/module/layout-machi" = {
    source = pkgs.fetchgit {
      url = "https://github.com/xinhaoyuan/layout-machi";
      rev = "3b3fcd82d7758f92acc601ed5e0b9d7612e318c7";
      sha256 = "sha256-7FbO6iPi+h2dKOHqldHa8ZcAH4zI48BVgcq1/Mza+T8=";
    };
    recursive = true;
  };
  home.file.".config/awesome/module/json.lua" = {
    source = (pkgs.fetchgit {
      url = "https://github.com/rxi/json.lua";
      rev = "dbf4b2dd2eb7c23be2773c89eb059dadd6436f94";
      sha256 = "sha256-BrM+r0VVdaeFgLfzmt1wkj0sC3dj9nNojkuZJK5f35s=";
    }) + "/json.lua";
  };
  home.file.".config/awesome/module/lua_pam" = {
    source = "${pkgs.lua-pam}";
    recursive = true;
  };
  systemd.user.services.gpaste = makeService {
    Type = "simple";
    ExecStart = "${pkgs.gnome.gpaste}/bin/gpaste-client start";
    Restart = "always";
  };
  systemd.user.services.ibus-daemin = makeService {
    Type = "simple";
    ExecStart = "/run/current-system/sw/bin/ibus-daemon -x -r -R";
    Restart = "always";
  };
  systemd.user.services.xiezuo = makeService {
    enable = false;
    Type = "simple";
    ExecStart = "${pkgs.xiezuo}/bin/xiezuo --no-sandbox --no-zygote --package-format=deb";
  };
  systemd.user.services.run_secret_script = makeService {
    Type = "simple";
    ExecStart = let script = pkgs.writeShellScriptBin "run_secret_script" ''
      if [[ -e /run/secrets/${config.home.username}/script ]]; then
        /run/secrets/${config.home.username}/script
      fi
    ''; in
      ''
        ${script}/bin/run_secret_script
      '';
  };
  systemd.user.services.run-secrets-scripts = {
    Unit = {
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
    Service = {
      Type = "simple";
      ExecStart =
        let
          script = pkgs.writeShellScriptBin "run-user-script" ''
            if [[ -e /run/secrets/${config.home.username}/script ]]; then
              /run/secrets/${config.home.username}/script
            fi
          ''; in
        ''
          ${script}/bin/run-user-script
        '';
    };
  };
  systemd.user.services.xhost = makeService {
    Type = "oneshot";
    ExecStart = "xhost +";
  };
  systemd.user.services.firefox = makeService {
    ExecStart = "${config.programs.firefox.package}/bin/firefox";
  };
  systemd.user.services.xpra-shadow = makeService {
    ExecStart =
      let
        script = pkgs.writeShellScriptBin "xpra-shadow" ''
          #!${pkgs.busybox}/bin/sh
          if command -v nvidia-smi; then
            xpra-html5-shadow $DISPLAY --bind-ws=0.0.0.0:$((${"\$"+"{DISPLAY:1}"}+6000)) --video-encoders=nvenc
          else
            xpra-html5-shadow $DISPLAY --bind-ws=0.0.0.0:$((${"\$"+"{DISPLAY:1}"}+6000))
          fi
        ''; in
      ''
        ${script}/bin/xpra-shadow
      '';
  };
  systemd.user.services.xpra-server = makeService {
    ExecStart =
      let
        script = pkgs.writeShellScriptBin "xpra-server" ''
          #!${pkgs.busybox}/bin/sh
          if command -v nvidia-smi ; then 
            xpra-html5-start :$((${"\$"+"{DISPLAY:1}"}+1000)) --bind-ws=0.0.0.0:$((${"\$"+"{DISPLAY:1}"}+7000)) --video-encoders=nvenc
          else 
            xpra-html5-start :$((${"\$"+"{DISPLAY:1}"}+1000)) --bind-ws=0.0.0.0:$((${"\$"+"{DISPLAY:1}"}+7000))
          fi
        ''; in
      ''
        ${script}/bin/xpra-server
      '';
  };
  systemd.user.services.lock = makeService {
    ExecStart =
      let
        lock_script = pkgs.writeShellScriptBin "lock" ''
          #!${pkgs.busybox}/bin/sh
          awesome-client " awesome.emit_signal ([[signal::lock]]) "
        ''; in
      ''
        ${pkgs.xautolock}/bin/xautolock -time 5 -locker ${lock_script}/bin/lock
      '';
  };
}
