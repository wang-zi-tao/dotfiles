{ config, pkgs, lib, ... }: {
  config = lib.mkIf config.cluster.nodeConfig.ollama.enable {
    services.ollama = {
      enable = true;
      host = "0.0.0.0";
      port = 11434;
      loadModels = [ "deepseek-r1:1.5b" "deepseek-coder:latest" ];
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
  };
}
