{ pkgs, ... }:
let hostname = "wangzi-pc"; in
{
  imports = [
    ../../module/cluster.nix
    ./fs.nix
    ./power.nix
    ./network.nix
    ./hardware-configuration.nix
  ];
  sops.defaultSopsFile = ../../secrets/wangzi-pc.yaml;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  sops.age.generateKey = true;
  sops.secrets."script" = {
    mode = "0500";
    restartUnits = [ "run-secrets-scripts" ];
  };
}
