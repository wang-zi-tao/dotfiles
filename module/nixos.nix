{
  config,
  pkgs,
  lib,
  nixpkgs,
  nixpkgs-unstable,
  nur,
  ...
}:
let
  hostName = config.networking.hostName;
  nodeConfig = config.cluster.nodes.${hostName};
  sops-enable = config.sops.defaultSopsFile != "/";
  # 可以通过 ssh 直接访问的节点（substituters 与 remote builder 的目标）
  ssh-nodes = lib.filterAttrs (
    node: wgConfig: config.cluster.nodes.${node}.sshd.enable
  ) config.cluster.wireguard.nodes;
  # nix-daemon 是 root 身份，而 root 根本没有 ssh 私钥（/root/.ssh 下只有 known_hosts），
  # 不显式指定身份就会 Permission denied（同样是 failed to start SSH connection）。
  # 用 nix-ssh 专用私钥，其公钥已在 secrets/public-key.yaml 的 ssh-public-keys 中授权；
  # 只有 root(daemon) 需要读它，保持 sops 默认的 0400 root:root 即可。
  ssh-substituter-key = config.sops.secrets."nix-ssh/private-key".path;
  # 没有 sops 的机器拿不到私钥，就没必要再挂这些注定连不上的 substituter
  ssh-substituters = lib.optionals sops-enable (
    lib.mapAttrsToList (
      node: wg: "ssh://nix-ssh@${wg.config.clusterIp}?ssh-key=${ssh-substituter-key}"
    ) ssh-nodes
  );
  # nix-daemon 是以 root 身份调用 ssh 的，root 的 ~/.ssh/known_hosts 里没有这些节点，
  # 于是每次 substitution 都会 Host key verification failed，
  # 表现为 error: failed to start SSH connection to '192.168.16.3'。
  # 写进全局 /etc/ssh/ssh_known_hosts，配合 /etc/ssh/ssh_config 中的
  # GlobalKnownHostsFile，对 root(daemon) 与普通用户一并生效。
  ssh-known-hosts = lib.listToAttrs (
    lib.concatLists (
      lib.mapAttrsToList (
        node: wgConfig:
        let
          hostNames = [
            wgConfig.config.clusterIp
            "${node}.wg"
            wgConfig.config.clusterIp2
            "${node}.wg1"
          ];
        in
        lib.imap0 (index: publicKey: {
          name = "${node}-${toString index}";
          value = {
            inherit hostNames publicKey;
          };
        }) config.cluster.nodes.${node}.ssh.hostPublicKeys
      ) ssh-nodes
    )
  );
in
{
  options =
    with lib;
    with lib.types;
    {
      lazyPackage = mkOption {
        type = listOf (oneOf [
          package
          str
        ]);
        default = [ ];
      };
      neovim.pkg = mkOption {
        type = package;
        default = pkgs.wangzi-neovim.override {
          enable-all = false;
        };
      };
    };
  config = lib.mkMerge [
    {
      programs.ssh.knownHosts = ssh-known-hosts;
      sops.secrets."nix-ssh/private-key" = lib.mkIf sops-enable {
        sopsFile = config.cluster.ssh.publicKeySops;
      };
      nix = {
        settings.substituters = [
          "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
          "https://nix-community.cachix.org"
          "https://nixpkgs-wayland.cachix.org"
          "https://mirrors.cernet.edu.cn/nix-channels/store"
          # "https://mirrors.ustc.edu.cn/nix-channels/store"
          # "https://mirror.sjtu.edu.cn/nix-channels/store"
          "https://cache.nixos.org/"
        ]
        ++ ssh-substituters;
        settings.trusted-substituters = [
          "https://hydra.nixos.org/"
        ]
        ++ ssh-substituters;
        settings.trusted-public-keys = [
          # "47.243.22.114:5000:wfL5ei3BfHGUVpiOihncv1LmbBzjqDm6uTFtJ95wueI="
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        ];
        settings.auto-optimise-store = true;
        settings.max-jobs = "auto";
        settings.stalled-download-timeout = 60;
        settings.require-sigs = false;
        daemonIOSchedClass = "idle";
        daemonCPUSchedPolicy = "batch";
        extraOptions = ''
          experimental-features = nix-command flakes
          # builders-use-substitutes = true
        '';
        gc.automatic = true;
        gc.dates = "weekly";
        gc.options = "-d --delete-older-than 7d";
        optimise.automatic = true;

        distributedBuilds = true;
        buildMachines = builtins.concatLists (
          lib.mapAttrsToList (
            host: node:
            lib.optional (node.buildNode.enable && host != hostName) {
              hostName = "${host}.wg";
              systems = [
                "x86_64-linux"
                "aarch64-linux"
              ];
              sshUser = "root";
              protocol = "ssh";
              maxJobs = 16;
              supportedFeatures = [
                "nixos-test"
                "benchmark"
                "big-parallel"
                "kvm"
              ];
              mandatoryFeatures = [ ];
              sshKey = "/home/wangzi/.ssh/id_rsa";
            }
          ) config.cluster.nodes
        );
      };

      nix.settings.trusted-users = [
        "root"
        "@wheel"
        "nix-ssh"
      ];
      users.groups.nix-ssh = { };
      users.users.nix-ssh = {
        description = "Nix SSH store user";
        isSystemUser = true;
        group = "nix-ssh";
        useDefaultShell = true;
      };

      networking.proxy.noProxy = "mirrors.tuna.tsinghua.edu.cn,mirrors.ustc.edu.cn,127.0.0.1,localhost";

      # environment.memoryAllocator.provider = "jemalloc";
      time.timeZone = "Asia/Shanghai";
      nix.nixPath = (lib.mapAttrsToList (name: value: "${name}=${value}") pkgs.flake-inputs) ++ [
        "nixpkgs=${nixpkgs}"
      ];
      nix.registry = (builtins.mapAttrs (name: value: { flake = nixpkgs; }) pkgs.flake-inputs) // {
        n.flake = nixpkgs;
        nixpkgs.flake = nixpkgs;
        u.flake = nixpkgs-unstable;
        unstable.flake = nixpkgs-unstable;
        nur.flake = nur;
      };
      system.autoUpgrade = {
        enable = false;
        flake = "github:wang-zi-tao/dotfiles";
        randomizedDelaySec = "30min";
        dates = "12:00";
      };
      system.stateVersion = "24.11";
      programs.nix-ld.enable = true;
      services.dbus.implementation = "broker";

      environment.systemPackages =
        with pkgs;
        [
          config.neovim.pkg
          nix
        ]
        ++ (builtins.map (
          pkg:
          if (builtins.typeOf pkg == "string") then
            if (lib.strings.hasPrefix "/" pkg) then
              pkgs.writeShellScriptBin (lib.lists.last (lib.strings.splitString "/" pkg)) "exec ${pkg} $@"
            else
              pkgs.writeShellScriptBin (lib.lists.last (lib.strings.splitString "." pkg)) ''nix run nixpkgs#"${pkg}" -- $@''
          else
            let
              name = if (builtins.hasAttr "pname" pkg) then pkg.pname else pkg.name;
            in
            pkgs.writeShellScriptBin name ''nix run nixpkgs#"${name}" -- $@''
        ) config.lazyPackage);

      services.nixfs.enable = true;
      services.swapspace.enable = true;
    }
    (lib.mkIf nodeConfig.guiClient.enable {
      hardware = {
        enableRedistributableFirmware = true;
        firmware = with pkgs; [
          broadcom-bt-firmware
        ];

        graphics = {
          enable = true;
          enable32Bit = true;
          extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
          extraPackages = with pkgs; [
            intel-vaapi-driver
            libvdpau-va-gl
            libva-vdpau-driver
          ];
        };
        bluetooth.enable = true;
        nvidia = {
          open = true;
          modesetting.enable = true;
          nvidiaSettings = true;
          forceFullCompositionPipeline = true;
        };
      };
      services.xserver = {
        modules = with pkgs; [
          xf86videonv
          xf86inputlibinput
          xf86videovesa
        ];
      };
    })
  ];

}
