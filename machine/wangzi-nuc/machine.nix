{ pkgs-template, nixpkgs, home-manager, sops-nix, nixseparatedebuginfod, ... }@inputs:
let
  hostname = "wangzi-nuc";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    ({ pkgs, ... }: {
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
      sops.secrets."script" = {
        mode = "0500";
        restartUnits = [ "run-secrets-scripts" ];
      };
    })
  ];
}
