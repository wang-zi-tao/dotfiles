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
          neovim-unwrapped = pkgs.unstable.neovim-unwrapped;
          enable-all = false;
        };
      };
    };
  config = lib.mkMerge [
    {
      nix = {
        settings.substituters = [
          "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
          "https://cache.nixos.org/"
          "https://nix-community.cachix.org"
          "https://nixpkgs-wayland.cachix.org"
          "https://mirrors.ustc.edu.cn/nix-channels/store"
          # "root@aliyun-hk:64022"
        ];
        settings.trusted-substituters = [
          "https://hydra.nixos.org/"
          # "ssh://root@aliyun-hk.wg:64022"
          # "ssh://root@aliyun-ecs.wg:64022"
          # "ssh://root@aliyun-hk:64022"
          # "ssh://root@aliyun-ecs:64022"
        ];
        settings.trusted-public-keys = [
          # "47.243.22.114:5000:wfL5ei3BfHGUVpiOihncv1LmbBzjqDm6uTFtJ95wueI="
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        ];
        settings.auto-optimise-store = true;
        settings.max-jobs = "auto";
        settings.stalled-download-timeout = 60;
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
              hostName = "${host}";
              system = "x86_64-linux";
              systems = [
                "x86_64-linux"
                "aarch64-linux"
              ];
              sshUser = "root";
              protocol = "ssh";
              maxJobs = 8;
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
      system.stateVersion = "22.11";
      programs.nix-ld.enable = true;

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
    }
    (lib.mkIf nodeConfig.guiClient.enable {
      hardware = {
        opengl.enable = true;
        enableRedistributableFirmware = true;
        enableAllFirmware = true;
        graphics = {
          enable = true;
          enable32Bit = true;
          extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
          extraPackages = with pkgs; [
            vaapiIntel
            libvdpau-va-gl
            vaapiVdpau
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
        verbose = 7;
        modules = with pkgs.xorg; [
          xf86videonv
          xf86inputlibinput
          xf86videovesa
        ];
      };
    })
  ];

}
