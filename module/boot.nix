{ config, pkgs, lib, ... }: {
  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.configurationLimit = 5;
    efi = {
      canTouchEfiVariables = false;
      efiSysMountPoint = "/boot/efi";
    };
    timeout = 1;
  };
  boot.kernelPackages = pkgs.linuxPackages_xanmod.extend (self: super: {
    virtualbox = super.virtualbox.override { inherit (self) kernel; };
  });
  # boot.kernelPackages = pkgs.linuxPackages_xanmod.extend (self: super: {
  # virtualbox = super.virtualbox.override { inherit (self) kernel; };
  # });
  boot.extraModulePackages = with config.boot.kernelPackages; [
    virtualbox
    # perf
    acpi_call
  ];
  boot.kernelParams = [ "quite" ];
  environment.systemPackages = with pkgs; [
    # config.boot.kernelPackages.perf
    perf-tools
    criu
    bcache-tools
    bcachefs-tools
  ];
  boot.kernel.sysctl = { "fs.file-max" = 6553560; };
  environment.etc."security/limits.conf".text = ''
    * soft nofile 65535   
    * hard nofile 65535
  '';
}
