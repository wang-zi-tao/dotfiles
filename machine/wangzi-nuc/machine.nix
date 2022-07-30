{ pkgs, ... }:
let hostname = "wangzi-nuc"; in
{
  imports = [
    ../../module/cluster.nix
    ./fs.nix
    ./network.nix
    ./hardware-configuration.nix
  ];
  sops.defaultSopsFile = ../../secrets/wangzi-nuc.yaml;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  sops.age.generateKey = true;
}
