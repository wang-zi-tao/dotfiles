{ pkgs, nixpkgs, home-manager, sops-nix, ... }@inputs:
let
  hostname = "wangzi-pc";
  system = "x86_64-linux";
in
nixpkgs.lib.nixosSystem {
  modules = [
    ../../module/cluster.nix

    ./fs.nix
    ./power.nix
    ./network.nix
    ./hardware-configuration.nix

    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    ({ pkgs, ... }: {
      sops.defaultSopsFile = ../../secrets/wangzi-pc.yaml;
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      sops.age.keyFile = "/var/lib/sops-nix/key.txt";
      sops.age.generateKey = true;
    })
  ];
  pkgs = pkgs system;
  inherit system;
}
