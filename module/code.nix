{ pkgs, config, lib, ... }:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes."${cfg.nodeName}";
in
with lib; with builtins; {
  config = lib.mkIf nodeConfig.CodeServer.enable {
    services.code-server = {
      enable = true;
      user = "root";
      host = "0.0.0.0";
      extraPackages = with pkgs;[ zsh tmux ];
      hashedPassword = "$argon2i$v=19$m=4096,t=3,p=1$tEUHulsHyVpSaxUJjUYoUw$LyAUrnqK5nx6F36mn3LWzrTP7xp4Ny5icZw1DiL1fs8";
    };
    services.caddy = lib.optionalAttrs nodeConfig.CodeServer.enable {
      enable = true;
      virtualHosts = {
        "https://${builtins.toString nodeConfig.publicIp}:4443" = {
          extraConfig = ''
            reverse_proxy http://localhost:4444
          '';
        };
      };
    };
  };
}
