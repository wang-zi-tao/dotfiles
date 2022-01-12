{ pkgs, config, lib, ... }:
let hostname = config.networking.hostName;
in {
  imports = [
    (pkgs.home-manager.nixosModules.home-manager ({
      home-manager.users.root = { ... }: {
        imports = [ ../../home-manager/terminal/terminal.nix ];
        inherit hostname;
      };
    }))
  ];
  users.users.root = { shell = pkgs.zsh; };
}
