{
  lib,
  rustPlatform,
}:
# cluster-node：集群网络自愈守护进程 + CLI。
# 由 flake.nix 里 packages 目录的自动 callPackage 发现，
# 对应 attrs: pkgs.cluster-node（同时提供 cluster-node / cluster-node-cli 两个二进制）。
rustPlatform.buildRustPackage {
  pname = "cluster-node";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./src
    ];
  };

  cargoLock.lockFile = ./Cargo.lock;
  # 单元测试只在开发时跑（nix 构建里跑一遍也可以，但依赖网络无关，这里保持与 homepage 一致）。
  doCheck = false;

  meta = {
    description = "集群网络自愈守护进程：局域网探测、拓扑同步、自动路由、WireGuard 隧道与集群 DNS";
    mainProgram = "cluster-node";
    platforms = lib.platforms.linux;
  };
}
