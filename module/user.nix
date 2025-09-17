{ config, pkgs, ... }@inputs:
let
  inherit (pkgs) lib;
  inherit (config.cluster) nodeConfig;
  sops-enable = config.sops.defaultSopsFile != "/";
in
{
  config = {
    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.backupFileExtension = ".backup";
    users.users.root = {
      shell = pkgs.nushell;
      hashedPassword = lib.mkDefault "$6$hKb9K3137LuNYU.n$ZZ0uRjIfMSOEyHtunuMorhVIo3QC3PlXgrHwW1Ysp3Bf4eqcouRoX20JHN4C.RYzlzow3MYu4GtugpJeOe0IB.";
    };
    home-manager.users.root =
      if !(nodeConfig.users ? root) then
        { imports = [ ../home-manager/profiles/root.nix ]; }
      else
        import nodeConfig.users.root;
    home-manager.users.wangzi = lib.mkIf (nodeConfig.users ? wangzi) (import nodeConfig.users.wangzi);
    users.groups.wangzi = lib.mkIf (nodeConfig.users ? wangzi) {
      gid = 1000;
      name = "wangzi";
    };
    users.users.wangzi = lib.mkIf (nodeConfig.users ? wangzi) {
      isNormalUser = true;
      uid = 1000;
      shell = pkgs.nushell;
      group = "wangzi";
      description = "王子陶";
      extraGroups = [
        "wheel"
        "networkmanager"
        "vboxusers"
        "docker"
        "lxd"
        "audio"
        "libvirtd"
        "disk"
      ];
      hashedPassword = lib.mkDefault "$6$Rd67.bPCRXvMahE1$seiawpNy.1eV/CLVBY5qogsP5Z77BIGMW2FvNf51XWi0QU597YpbnfaNjTwQQxKA3mSwBV47dxlkJmqyX1y5x1";
    };
    sops.secrets."env.json" = lib.mkIf (sops-enable && (nodeConfig.users ? wangzi)) {
      owner = "root";
      mode = "0555";
    };
  };
}
