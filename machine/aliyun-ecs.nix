# { pkgs, home-manager, ... }:
# home-manager.lib.homeManagerConfiguration {
  # configuration = {
    # imports = [ ../home-manager/terminal/terminal.nix ];
    # nixpkgs.config = { modules = [ ../module/nixos.nix ]; };
  # };
  # system = "x86_64-linux";
  # username = "root";
  # homeDirectory = "/root";
# }
{ pkgs, nixpkgs, home-manager, ... }:
nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ../../module/nixos.nix
    ../../module/boot.nix
    ../../module/terminal.nix
    ../../services/sshd.nix
    ../../services/container.nix

    home-manager.nixosModules.home-manager
    ({
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.wangzi = { ... }: {
        imports = [
          ../../home-manager/terminal/terminal.nix
        ];
      };
      home-manager.users.root = { ... }: {
        imports = [ ../../home-manager/terminal/terminal.nix ];
      };
    })
    ({ pkgs, ... }: {
      users.groups.wangzi = {
        gid = 1000;
        name = "wangzi";
      };
      users.users.root = { shell = pkgs.zsh; };
    })
  ];
  inherit pkgs;
}
