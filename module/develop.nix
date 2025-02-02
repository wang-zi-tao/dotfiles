{ config, pkgs, lib, ... }:
let sops-enable = config.sops.defaultSopsFile != "/";
in {
  config = lib.mkIf config.cluster.nodeConfig.develop.enable {
    services.nixseparatedebuginfod.enable = true;
    environment.systemPackages = with pkgs; [ config.boot.kernelPackages.perf ];
    lazyPackage = with pkgs; [ perf-tools gperftools criu ];
    programs.criu.enable = true;
    networking.firewall.allowedTCPPorts = [ 1716 ];
    environment.etc.nixos = { source = ../.; };
    systemd.coredump = {
      enable = true;
      extraConfig = ''
        Storage=journal
      '';
    };
    sops.secrets."openai/user" = lib.mkIf sops-enable {
      sopsFile = config.cluster.ssh.publicKeySops;
      mode = "0555";
    };
    sops.secrets."openai/key" = lib.mkIf sops-enable {
      sopsFile = config.cluster.ssh.publicKeySops;
      mode = "0555";
    };
  };
}
