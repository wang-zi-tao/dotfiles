{
  config,
  pkgs,
  lib,
  kubenix,
  ...
}:
let
  hostname = config.networking.hostName;
  nodeList = builtins.attrValues config.cluster.nodes;
  wireguardCluster = config.cluster.wireguard.edges;
  inherit (config.cluster) nodeConfig;
in
{
  options = {
    k3s.resources = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = "Resources for k3s server and agent.";
    };
  };
  config = lib.mkMerge [
    (lib.mkIf config.cluster.nodeConfig.container.enable {
      environment.etc."docker/daemon.json".text = ''
        {
          "registry-mirrors": [
            "https://mirror.ccs.tencentyun.com",
            "https://registry.docker-cn.com",
            "https://registry.cn-hangzhou.aliyuncs.com"
          ]
        }
      '';
      virtualisation = {
        podman = {
          enable = true;
          # enableNvidia = config.services.xserver.enable;
        };
        docker = {
          enable = true;
          enableOnBoot = lib.mkDefault false;
          # enableNvidia = config.services.xserver.enable;
        };
      };
      programs.criu.enable = true;
      lazyPackage = with pkgs; [ criu ];
    })
    (lib.mkIf nodeConfig.k3s.enable {
      environment.systemPackages = with pkgs; [
        k3s
      ];
      sops.secrets."k3s-token" = {
        mode = "0500";
        restartUnits = [ "k3s" ];
        sopsFile = config.cluster.ssh.publicKeySops;
      };
      environment.etc."rancher/k3s/registries.yaml".text = ''
        mirrors:
          "*":
            endpoint:
              - "https://registry.cn-hangzhou.aliyuncs.com"
      '';
      systemd.services.k3s = {
        enable = true;
        wantedBy = [ "multi-user.target" ];
        path = [
          pkgs.wireguard-tools
          pkgs.busybox
          pkgs.bash
          pkgs.scripts."wg-add.sh"
        ];
        script =
          let
            clusterIp = wireguardCluster.${hostname}.config.clusterIp;
            servers = lib.optionalString (nodeConfig.k3s.kind == "agent") (
              builtins.concatStringsSep " " (
                builtins.map (node: "--server https://${wireguardCluster.${node.hostname}.config.clusterIp}:6443") (
                  builtins.filter (node: node.k3s.enable && node.k3s.kind == "server") nodeList
                )
              )
            );
            node-taint = lib.optionalString (
              nodeConfig.k3s.taint != null
            ) "--node-taint ${nodeConfig.k3s.taint}";
            commonArgs = ''
              ${node-taint} \
              --node-external-ip ${clusterIp} \
              --bind-address ${clusterIp} \
              --token-file ${config.sops.secrets."k3s-token".path}
            '';
          in
          if nodeConfig.k3s.kind == "server" then
            ''
              ${pkgs.k3s}/bin/k3s server \
                        --flannel-backend=wireguard-native \
                        --write-kubeconfig /etc/rancher/k3s/k3s.yaml \
                        --system-default-registry "registry.cn-hangzhou.aliyuncs.com" \
                        ${commonArgs}
            ''
          else
            ''
              ${pkgs.k3s}/bin/k3s agent \
                        ${servers} \
                        ${commonArgs}
            '';
        serviceConfig = {
          Type = "simple";
          Restart = "always";
          RestartSec = "2s";
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];
          ExecStartPre = "${pkgs.busybox}/bin/mkdir -p /var/lib/rancher/k3s/server/tls";
        };
        unitConfig = {
          RequiresMountsFor = "/etc/rancher/k3s/registries.yaml";
        };
      };
      security.pki.certificates = [
        ''
          -----BEGIN CERTIFICATE-----
          MIIBeDCCAR2gAwIBAgIBADAKBggqhkjOPQQDAjAjMSEwHwYDVQQDDBhrM3Mtc2Vy
          dmVyLWNhQDE3NDkwNTM4NjIwHhcNMjUwNjA0MTYxNzQyWhcNMzUwNjAyMTYxNzQy
          WjAjMSEwHwYDVQQDDBhrM3Mtc2VydmVyLWNhQDE3NDkwNTM4NjIwWTATBgcqhkjO
          PQIBBggqhkjOPQMBBwNCAASON7ssdHOlf5W57Xp7wFDnK3uQ/M05qJ7BTKzQyPBx
          +e10MrMGNVrb7II9phTfLo/4vZa/Mr4BtX7+rwdERLt/o0IwQDAOBgNVHQ8BAf8E
          BAMCAqQwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQU9oyZZ3hAwTDynprbAy9C
          AO8sC78wCgYIKoZIzj0EAwIDSQAwRgIhAPEf2ENoNJB4tMdHuHNCwbTnkiSmVC3a
          m58Yft3WkxQfAiEAsLBwcRLpTrwMOV66vxcMhmU5tiETNUBSKWHpxABo9D4=
          -----END CERTIFICATE-----
        ''
      ];
    })
    (lib.mkIf (false && nodeConfig.k3s.enable && nodeConfig.k3s.kind == "server") {
      environment.etc."kubenix.yaml".source =
        (kubenix.evalModules.${pkgs.system} {
          module =
            { kubenix, ... }:
            {
              imports = [ kubenix.modules.k8s ];
              kubernetes.resources = config.k3s.resources;
            };
        }).config.kubernetes.resultYAML;

      system.activationScripts.kubenix.text = ''
        ln -sf /etc/kubenix.yaml /var/lib/rancher/k3s/server/manifests/kubenix.yaml
      '';
    })
  ];
}
