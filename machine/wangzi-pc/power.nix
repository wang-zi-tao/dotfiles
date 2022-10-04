{ config, pkgs, lib, ... }: {
  services = {
    power-profiles-daemon.enable = false;
    tlp = {
      enable = false;
      settings = {
        USB_BLACKLIST = "248a:8368";
        PLATFORM_PROFILE_ON_BAT = "low-power";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
        DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth wifi wwan";
      };
    };
  };
  services.logind.lidSwitch = "ignore";
}
