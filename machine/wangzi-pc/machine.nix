{ pkgs-template, nixpkgs, modules, ... }@inputs:
let
  hostname = "wangzi-pc";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = modules ++ [
    ({ pkgs, ... }: {
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
      networking.firewall.allowedTCPPorts = [ 11434 ];
      services.ollama.acceleration = "cuda";
    })
  ];
}
