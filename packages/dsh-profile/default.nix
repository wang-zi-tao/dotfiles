{
  lib,
  stdenvNoCC,
  nodejs,
  pnpm,
  pnpmConfigHook,
  fetchPnpmDeps,
  git,
  dsh-hindsight,
  dsh-cordis-patch,
  formats,
}:
{
  src,
  hash,
  name ? [ ],
  plugins ? [ ],
  cordis_patch ? [ ],
  package,
}:

let
  # Nix-built dsh plugins injected into this profile's node_modules and
  # bundle list. Add new entries here as more plugins move into packages/.
  pluginArgs = lib.escapeShellArgs (map toString plugins);
  # formats.yaml（remarshal/json2yaml）给每个字符串都加引号，插件里那些
  # `!!js …` 表达式会退化成普通字符串，dsh 的 Loader 就不再求值了。
  # dsh-cordis-patch 用同一套 schema 做往返：读出来，再把它们裸写成 YAML 标签。
  # formats.yaml 的产物是 store 路径，直接交给转换包读，不经过 builtins.readFile，
  # 所以不会额外引入 IFD。
  cordis_patch_yaml = dsh-cordis-patch {
    yaml = (formats.yaml { }).generate "cordis.raw.yml" cordis_patch;
  };
in
stdenvNoCC.mkDerivation {
  pname = "dsh-profile-${name}";
  version = "0.1.0";

  src = src;

  nativeBuildInputs = [
    nodejs
    pnpm
    pnpmConfigHook
    git
  ];

  buildInputs = plugins ++ [ package ];

  pnpmDeps = fetchPnpmDeps {
    pname = "dsh-profile-deps-${name}";
    src = src;
    # First build fails with a hash mismatch; fill in the printed sha256 here.
    hash = hash;
    fetcherVersion = 4;
    nativeBuildInputs = [ git ];
  };

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib"
    cp -r package.json pnpm-lock.yaml pnpm-workspace.yaml "$out"
    cp -r node_modules $out/lib
    ln -s ${cordis_patch_yaml} $out/cordis.patch.yml

    node ${./generate-bundles.mjs} "$out/package.json" "$out/lib/node_modules" ${pluginArgs}

    # 原生解析回退层。dsh 的运行时解析器只把一部分 @deepseek-ai/dsh-* 放进它自己的
    # entries 表（installation scope），表里没有的名字会退化成 Node 原生解析，
    # 而原生解析是从模块所在的 /nix/store/<profile>/lib/node_modules/... 逐级向上找
    # node_modules，于是会走到 $out/node_modules。这里指向 dsh 应用自己的依赖集，
    # 等价于 npm 安装时 node_modules/@deepseek-ai/dsh/node_modules 的自然布局。
    # 例：@deepseek-ai/dsh-session-persistence-jsonl 不在 entries 里，但在这里有；
    # 缺了它 dsh-tui 会 ERR_MODULE_NOT_FOUND，连带 agent-team 一起挂。
    ln -s "${package}/lib/node_modules/@deepseek-ai/dsh/node_modules" "$out/"
    runHook postInstall
  '';

  meta = with lib; {
    description = "Nix-built dsh web profile: pnpm-managed plugins as a read-only store layer";
    platforms = platforms.unix;
  };
}
