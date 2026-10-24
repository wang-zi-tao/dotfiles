{
  lib,
  stdenv,
  rustPlatform,
  buildNpmPackage,
}:
# homepage：Rust/Axum 后端 + Vue 前端，产物组装成单一目录：
#   $out/bin/homepage                 后端（同时带 hash-password / convert-infinity 子命令）
#   $out/share/homepage/web/          前端构建产物
#   $out/share/homepage/bookmarks.json 初始收藏（由 Infinity 导出转换而来，可手工编辑）
let
  version = "0.1.0";

  rustSrc = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./src
      ./migrations
    ];
  };

  webSrc = lib.fileset.toSource {
    root = ./web;
    fileset = lib.fileset.unions [
      ./web/package.json
      ./web/package-lock.json
      ./web/index.html
      ./web/vite.config.ts
      ./web/tsconfig.json
      ./web/src
    ];
  };

  backend = rustPlatform.buildRustPackage {
    pname = "homepage";
    inherit version;
    src = rustSrc;
    cargoLock.lockFile = ./Cargo.lock;
    # SQLite 通过 libsqlite3-sys 的 bundled feature 静态编译，不需要 pkg-config。
    doCheck = false;
  };

  frontend = buildNpmPackage {
    pname = "homepage-web";
    inherit version;
    src = webSrc;
    npmDepsHash = "sha256-VN9mejKQe3FDQAx2L8Gaq9QGOKRN7E5NYoXhQUPLAT8=";
    npmBuildScript = "build";
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r dist/. $out/
      runHook postInstall
    '';
  };
in
stdenv.mkDerivation {
  pname = "homepage";
  inherit version;

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin $out/share/homepage
    install -Dm755 ${backend}/bin/homepage $out/bin/homepage
    cp -r ${frontend} $out/share/homepage/web
    install -Dm644 ${./bookmarks.json} $out/share/homepage/bookmarks.json
    runHook postInstall
  '';

  meta = {
    description = "登录后才可访问的个人浏览器主页（收藏 + 内网入口 + 搜索）";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
