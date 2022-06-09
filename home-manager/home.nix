{ nixosConfig, pkgs, lib, config, ... }:
let cfg-node = config.cluster.nodes."${cfg.cluster.nodeName}";
in
with lib; {
  imports =
    [ (mkIf cfg-node.my-shell ../../home-manager/terminal/terminal.nix) ]
    ++ mkIf (config.home.username != "root") [
      (mkIf cfg-node.develop ../../home-manager/develop/develop.nix)
      (mkIf cfg-node.guiServer ../../home-manager/desktop/desktop.nix)
      (mkIf cfg-node.guiServer ../../home-manager/application/application.nix)
    ];
}
