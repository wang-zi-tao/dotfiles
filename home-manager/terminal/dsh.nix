{
  pkgs,
  lib,
  inputs,
  config,
  ...
}:
let
  dsh-home-config = pkgs.dsh-home.home-config-factory {
    dsh-home = pkgs.dsh-home.override (old: {
      enable_develop = config.dsh.develop;
      enable_desktop = config.dsh.desktop;
    });
    lib = lib;
  };
in
{
  config = {

    systemd.user.services.dsh = {
      Unit = {
        Description = "dsh server";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.dsh}/bin/dsh web --no-open";
        Restart = "always";
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

    home = dsh-home-config.home // {
      packages = with pkgs; [
        bubblewrap

        (writeScriptBin "dsh" ''
          #!${bash}/bin/bash
          if [ -e /run/secrets/apikey/deepseek ]; then
            export DEEPSEEK_API_KEY=$(cat /run/secrets/apikey/deepseek)
          fi
          export DSH_TUI_SKIP_UPDATE=1
          exec ${dsh}/bin/dsh "$@"
        '')
      ];
    };
  };
}
