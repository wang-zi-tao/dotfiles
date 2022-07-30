{ pkgs, config, lib, ... }:
let
  cfg = config.cluster;
  nodeConfig = cfg.nodes.${cfg.nodeName};
  networkConfig = config.cluster.network.nodes.${config.cluster.nodeName}.config;
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
        "https://${builtins.toString networkConfig.publicIp}:43243" = {
          extraConfig = ''
            @folder {
              query folder=*
            }
            @type {
              query type=*
            }
            reverse_proxy @folder http://localhost:4444
            reverse_proxy @type http://localhost:4444
            reverse_proxy /fake.html http://localhost:4444
            reverse_proxy /update/* http://localhost:4444
            reverse_proxy /static/* http://localhost:4444
            reverse_proxy /webview/* http://localhost:4444
            reverse_proxy /vscode-remote-resource http://localhost:4444
            # reverse_proxy http://localhost:4444
          '';
        };
      };
    };
    networking.firewall.allowedUDPPorts = [ 43243 ];
    networking.firewall.allowedTCPPorts = [ 43243 ];
  };
}
