{
  fetchFromGitHub,
  fetchPnpmDeps,
  nodejs,
  pnpm,
  pnpmConfigHook,
  pnpmBuildHook,
  stdenv,
  git,
  lib,
  nodejsForDsh,
  makeWrapper,
  stdenvNoCC,
}:
let
  pname = "dsh";
  version = "v0.2.0-rc.2";
  src = fetchFromGitHub {
    owner = "deepseek-ai";
    repo = "deepseek-harness";
    rev = "dsh-${version}";
    sha256 = "sha256-VUANHGBhUEgV/MC4Vart49fILOUN43WjfgU7ArPpuxM=";
    leaveDotGit = true;
  };

  # ── 产物裁剪 ────────────────────────────────────────────────────────────────
  # pnpm 工作区 install 会把「每个工程声明的全部东西」都落盘：各包自己的
  # devDependencies（typescript / vitest / playwright / oxlint / electron-winstaller…）、
  # 工具链、只在前端打包期用到的浏览器库，以及完全不属于本产品的可选 provider。
  # 这些都不在运行时进程的可达依赖里，但 installPhase 是 `cp -r *` 整个树拷贝，
  # 于是全部进了 store。prune-deps.mjs 在拷贝前重算一遍依赖图并删掉不可达部分。
  #
  # 1) 剔除产品不需要的可选 subagent provider（及其独占的官方 SDK）：
  #    Codex 走 @openai/codex，Claude Code 走 @anthropic-ai/claude-agent-sdk
  #    （单是 claude-agent-sdk 的 linux-x64 平台载荷就 ~216 MB）。两者都只是
  #    可选 Bundle，需要时由 profile 自己 `dsh plugin add` 装配，生产 dsh 默认不装。
  #    包目录不拷贝 → 它们的 npm 依赖因不可达而随闭包一起消失。
  #    注意 @anthropic-ai/sdk 本体不在这里：它是 @earendil-works/pi-ai 的
  #    anthropic-messages provider 依赖（bundle/base 链路），删了会伤到 LLM 侧。
  prunedPackages = [
    "@deepseek-ai/dsh-subagent-codex"
    "@deepseek-ai/dsh-subagent-claude-code"
    "@openai/codex"
    "@anthropic-ai/claude-agent-sdk"
  ];
  pnpmDeps = (
    fetchPnpmDeps {
      pname = "dsh-deps-${stdenvNoCC.system}";
      src = src;
      fetcherVersion = 4;
      hash = "sha256-l+q7KENUblSgE/vE+vNtDl+bRMqTa61AFsu7/lTnS2U=";
      nativeBuildInputs = [
      ];
      prePnpmInstall = ''
        echo "fetchTimeout: 6000000" >> pnpm-workspace.yaml
      '';
    }
  );

  # 可选再加码（默认关）：
  #   stripSourceMaps → 去掉 packages/*/lib 下的 source map（约 60 MB），
  #                     代价是堆栈无法还原到 TS 源码行号；
  #   stripRepoDocs   → 去掉 docs/ snapshots/ website/ benchmarks/（约 30 MB）。
  stripSourceMaps = false;
  stripRepoDocs = false;

  # 保底的「运行期工具链」：dsh 进程会现场 require 它们（HMR 重新打包客户端插件、
  # webworker 打包等），但上游把这些工具声明成 devDependency，生产闭包遍历看不到
  # ——只按声明裁剪就会把它们删掉，运行时表现为「找不到 … 的 prebuild / 原生绑定」。
  # 这里按名字前缀显式保留（连带保留它们自己的生产依赖），代价约 60 MB；
  # 确认用不到就把 keepRuntimeToolchain 设成 false 把这 60 MB 省回来。
  keepRuntimeToolchain = true;
  runtimeToolchain = [
    "esbuild"
    "@esbuild"
    "vite"
    "rolldown"
    "@rolldown"
    "rollup"
    "@rollup"
  ];

  pruneFlags = [
    # 只保留本机平台的原生载荷：node-pty 自带的 win32/darwin prebuilds、
    # native/ 下提交的其他平台 addon 都会被删掉。
    "--keep-arch"
    "${stdenv.hostPlatform.node.platform}-${stdenv.hostPlatform.node.arch}"
    # workspace 包的 tests/ 目录（运行时无引用，已核对仅出现在注释里）。
    "--strip-tests"
  ]
  ++ lib.optional stripSourceMaps "--strip-maps"
  ++ lib.optional stripRepoDocs "--strip-docs"
  ++ lib.optionals keepRuntimeToolchain (
    lib.concatMap (name: [
      "--keep"
      name
    ]) runtimeToolchain
  )
  ++ lib.concatMap (name: [
    "--exclude"
    name
  ]) prunedPackages;
  pruneArgs = lib.escapeShellArgs pruneFlags;
in
stdenv.mkDerivation {
  inherit
    pname
    version
    src
    pnpmDeps
    ;
  nativeBuildInputs = [
    nodejs
    pnpm
    pnpmConfigHook
    pnpmBuildHook
    git
    makeWrapper
  ];
  # pnpm 的 node_modules 不是 npm 布局，nixpkgs 的 npm prune 用不了；
  # 生产裁剪由下面 installPhase 里的 prune-deps.mjs 负责。
  dontNpmPrune = true;
  dontCheckForBrokenSymlinks = true;

  npmWorkspace = "dsh";

  installPhase = ''
    runHook preInstall

    # # 先收缩到运行闭包再拷贝（1.9 GB → ~0.9 GB），详见 prune-deps.mjs 顶部注释。
    node ${./prune-deps.mjs} "$PWD" ${pruneArgs}

    root=$out/lib/node_modules/@deepseek-ai/dsh-root
    mkdir -p $root
    cp -r * $root

    dsh_package=$out/lib/node_modules/@deepseek-ai/dsh/
    mkdir -p $dsh_package
    ln -s $root/apps/cli/node_modules $dsh_package

    mkdir -p $out/bin
    makeWrapper ${nodejsForDsh.out}/bin/node $out/bin/dsh \
      --argv0 dsh \
      --add-flags "--expose-internals" \
      --add-flags "$root/apps/cli/lib/bin.js"
  '';
}
