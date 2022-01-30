{ pkgs, nixpkgs, home-manager, sops-nix, ... }@inputs:
let hostname = "wangzi-pc";
in nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ../../module/cluster.nix

    ./fs.nix
    ./power.nix
    ./network.nix
    ./hardware-configuration.nix

    # ../../module/user.nix
   # ({ config, ... }@input:
      # import ../../module/user.nix { inherit pkgs config; })
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    ({ pkgs, ... }: {
      sops.defaultSopsFile = ../../secrets/wangzi-pc.yaml;
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      sops.age.keyFile = "/var/lib/sops-nix/key.txt";
      sops.age.generateKey = true;
    })
  ];
  inherit pkgs;
}
