{ pkgs-template, nixpkgs, home-manager, sops-nix, nixseparatedebuginfod, nixfs, ... }@inputs:
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
    nixseparatedebuginfod.nixosModules.default
    nixfs.nixosModules.nixfs
    ({ pkgs, ... }: {
      imports = [
        ../../module/cluster.nix
        ./fs.nix
        ./network.nix
        ./hardware-configuration.nix
      ];
      services.nixseparatedebuginfod.enable = true;
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
      hardware.opengl.package = (pkgs.enableDebugging pkgs.mesa).drivers;
      hardware.opengl.driSupport32Bit = true;
    })

  ];
}
