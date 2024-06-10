{ pkgs-template, nixpkgs, home-manager, sops-nix, nixfs, ... }@inputs:
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
    nixfs.nixosModules.nixfs
    ({ pkgs, ... }: {
      imports = [
        ../../module/cluster.nix
        ./fs.nix
        ./network.nix
        ./hardware-configuration.nix
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
      environment.systemPackages = with pkgs;[
        gtk3
        glibc
        glib
        cairo
        gobject-introspection
      ];
      hardware.opengl.driSupport32Bit = true;
      services.ollama = {
          enable = true;
          listenAddress = "0.0.0.0:11434";
          # acceleration = "rocm";
          environmentVariables = {
              HTTP_PROXY = "http://aliyun-hk.wg:8889";
              HTTPS_PROXY = "http://aliyun-hk.wg:8889";
          };
      };
    })
  ];
}
