{
  pkgs,
  lib,
  config,
  ...
}:
let
  makeService = service: {
    Unit = {
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
    Service = service // {
      Type = "simple";
    };
  };
in
with builtins;
{
  options = with lib; {
    services.rustdesk.enable = mkOption {
      type = types.bool;
      default = false;
    };
    desktop.lock-timeout = mkOption {
      type = types.int;
      default = 30;
    };
    desktop.close-screen-timeout = mkOption {
      type = types.int;
      default = 40;
    };
  };
  config = {
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
      source =
        (pkgs.fetchgit {
          url = "https://github.com/rxi/json.lua";
          rev = "dbf4b2dd2eb7c23be2773c89eb059dadd6436f94";
          sha256 = "sha256-BrM+r0VVdaeFgLfzmt1wkj0sC3dj9nNojkuZJK5f35s=";
        })
        + "/json.lua";
    };
    home.file.".config/awesome/module/lua_pam" = {
      source = "${pkgs.lua-pam}";
      recursive = true;
    };
    systemd.user.services.gpaste = makeService {
      Type = "simple";
      ExecStart = "${pkgs.gpaste}/bin/gpaste-client start";
      Restart = "always";
    };
    systemd.user.services.ibus-daemin = makeService {
      Type = "simple";
      ExecStart = "/run/current-system/sw/bin/ibus-daemon -x -r -R";
      Restart = "always";
    };
    # systemd.user.services.xiezuo = makeService {
    #   ExecStart = "${pkgs.xiezuo}/bin/xiezuo --no-sandbox --no-zygote --package-format=deb";
    # };
    systemd.user.services.virt-manager = makeService { ExecStart = "virt-manager"; };
    systemd.user.services.barrier = makeService {
      Type = "simple";
      ExecStart = "${pkgs.barrier}/bin/barrier --config ${config.home.homeDirectory}/.barrier";
    };
    systemd.user.services.run_secret_script = makeService {
      ExecStart =
        let
          script = pkgs.writeShellScriptBin "run_secret_script" ''
            if [[ -e /run/secrets/${config.home.username}/script ]]; then
              /run/secrets/${config.home.username}/script
            fi
          '';
        in
        ''
          ${script}/bin/run_secret_script
        '';
    };
    systemd.user.services.run-secrets-scripts = {
      Unit = {
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart =
          let
            script = pkgs.writeShellScriptBin "run-user-script" ''
              if [[ -e /run/secrets/${config.home.username}/script ]]; then
                /run/secrets/${config.home.username}/script
              fi
            '';
          in
          ''
            ${script}/bin/run-user-script
          '';
      };
    };
    systemd.user.services.xhost = makeService {
      Type = "oneshot";
      ExecStart = "xhost +";
    };
    systemd.user.services.xpra-shadow = makeService {
      Restart = "always";
      ExecStart =
        let
          xpra-base-command = "${pkgs.xpra}/bin/xpra shadow $DISPLAY --bind-ws=0.0.0.0:$((${"\$" + "{DISPLAY:1}"}+6000)),auth=sys --no-daemon";
          script = pkgs.writeShellScriptBin "xpra-shadow" ''
            #!${pkgs.busybox}/bin/sh
            if command -v nvidia-smi; then
              ${xpra-base-command} --video-encoders=nvenc
            else
              ${xpra-base-command}
            fi
          '';
        in
        ''
          ${script}/bin/xpra-shadow
        '';
    };
    systemd.user.services.xpra-server = makeService {
      ExecStart =
        let
          startup-command = pkgs.writeScriptBin "xpra-start.sh" ''
            #!${pkgs.stdenv.shell}
            ${pkgs.alacritty}/bin/alacritty -e alacritty -e tmux attach -t dev
          '';
          xpra-base-command = ''${pkgs.xpra}/bin/xpra start :$((${"\$" + "{DISPLAY:1}"}+1000)) --bind-ws=0.0.0.0:$((${"\$" + "{DISPLAY:1}"}+7000)),auth=sys --exec-wrapper="vglrun" --no-daemon --start=${startup-command}'';
          script = pkgs.writeShellScriptBin "xpra-server" ''
            if command -v nvidia-smi ; then 
              ${xpra-base-command} --video-encoders=nvenc
            else 
              ${xpra-base-command}
            fi
          '';
        in
        ''
          ${script}/bin/xpra-server
        '';
    };
    systemd.user.services.close-screen = makeService {
      ExecStart = ''${pkgs.xorg.xset}/bin/xset s ${toString (config.desktop.close-screen-timeout * 60)}'';
    };
    systemd.user.services.lock = makeService {
      ExecStart =
        let
          lock_script = pkgs.writeShellScriptBin "lock" ''
            #!${pkgs.busybox}/bin/sh
            awesome-client " awesome.emit_signal ([[signal::lock]]) "
          '';
        in
        ''
          ${pkgs.xautolock}/bin/xautolock -time ${toString config.desktop.lock-timeout} -locker ${lock_script}/bin/lock
        '';
    };
    systemd.user.services.rustdesk = lib.mkIf config.services.rustdesk.enable (makeService {
      Type = "simple";
      ExecStart = "${pkgs.rustdesk}/bin/rustdesk";
      Restart = "always";
    });
    home.packages = with pkgs;[ virtualgl ];
  };
}
