{
  config,
  pkgs,
  lib,
  ...
}:
let
  sops-enable = config.sops.defaultSopsFile != "/";
in
{
  config = lib.mkIf config.cluster.nodeConfig.ollama.enable {
    services.ollama = {
      enable = true;
      host = "0.0.0.0";
      port = 11434;
      loadModels = [
        "deepseek-r1:8b"
      ];
      environmentVariables = {
        # HTTP_PROXY = "http://aliyun-hk.wg:8889";
        # HTTPS_PROXY = "http://aliyun-hk.wg:8889";
      };
    };
    services.nextjs-ollama-llm-ui = {
      enable = true;
      hostname = "0.0.0.0";
      port = 11435;
    };
    sops.secrets.ai = lib.mkIf sops-enable {
      sopsFile = config.cluster.ssh.publicKeySops;
      mode = "0555";
    };
  };
}
