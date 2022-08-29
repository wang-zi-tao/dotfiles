{ pkgs, config, lib, ... }: {
  home.sessionVariables = with pkgs; {
    BOMB = "1";
  };
  home.packages = with pkgs;[ noto-fonts-cjk-sans ];
  fonts.fontconfig.enable = true;
  systemd.user.timers.wsl-clean = {
    Unit = {
      Description = "WSL clean";
      Requires = "wsl-clean.services";
    };
    Timer = {
      Unit = "wsl-clean.service";
      OnCalendar = "*-*-* *:00:00";
    };
    Install = { WantedBy = [ "timers.target" ]; };
  };
  systemd.user.services.wsl-clean = {
    Unit = {
      Description = "WSL clean";
      Wants = "wsl-clean.timer";
    };
    Services = {
      Type = "oneshot";
      ExecStart = "echo 3 >> /proc/sys/vm/drop_caches";
    };
    Install = { WantedBy = [ "multi-user.target" ]; };
  };
}
