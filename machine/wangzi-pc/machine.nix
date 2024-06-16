{
  pkgs-template,
  nixpkgs,
  home-manager,
  sops-nix,
  disko,
  ...
}@inputs:
let
  hostname = "wangzi-pc";
  system = "x86_64-linux";
  pkgs = pkgs-template system;
in
nixpkgs.lib.nixosSystem {
  inherit pkgs system;
  specialArgs = inputs;
  modules = [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    disko.nixosModules.disko
    (
      { pkgs, ... }:
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
        networking.firewall.allowedTCPPorts = [ 11434 ];
        services.ollama = {
          enable = true;
          listenAddress = "0.0.0.0:11434";
          acceleration = "cuda";
          environmentVariables = {
            HTTP_PROXY = "http://aliyun-hk.wg:8889";
            HTTPS_PROXY = "http://aliyun-hk.wg:8889";
          };
        };
      }
    )
  ];
}
