{ pkgs-template, nixpkgs, modules, ... }@inputs:
let
  hostname = "wangzi-nuc";
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
        ./network.nix
        ./hardware-configuration.nix
        ./virtualisation.nix
      ];
      services.nixfs.enable = true;
      sops.defaultSopsFile = ../../secrets/wangzi-nuc.yaml;
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      sops.age.keyFile = "/var/lib/sops-nix/key.txt";
      sops.age.generateKey = true;
      sops.secrets."script" = {
        mode = "0500";
        restartUnits = [ "run-secrets-scripts" ];
      };

      environment.enableDebugInfo = true;
      environment.systemPackages = with pkgs; [
        gtk3
        glibc
        glib
        cairo
        gobject-introspection
      ];
      services.ollama.acceleration = "rocm";
      hardware.graphics.extraPackages = with pkgs; [ rocmPackages.clr.icd ];

      vm = {
        guest-reserved = 1600;
        host-reserved = 1600;
        guest-reserved-percent = 0.2;
      };
    })
  ];
}
