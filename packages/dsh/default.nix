{
  fetchFromGitHub,
  fetchPnpmDeps,
  nodejs-slim,
  pnpm,
  pnpmConfigHook,
  pnpmBuildHook,
  stdenv,
  git,
  lib,
  bashInteractive,
  makeWrapper,
}:
let
  pname = "dsh";
  version = "v0.1.7-rc.2";
  src = fetchFromGitHub {
    owner = "deepseek-ai";
    repo = "deepseek-harness";
    rev = "dsh-${version}";
    sha256 = "sha256-hVwVPqk4SOfP/GG+EM4AmfAQG3hV0iKiMOd1xFMGOKE=";
    leaveDotGit = true;
  };
  pnpmDeps =
    (fetchPnpmDeps {
      pname = "dsh-deps";
      src = src;
      fetcherVersion = 4;
      hash = "sha256-vCaIyF5r9vXYXmo2SIxSKzHZpj7b2byP6rLfkoo0P34=";
      nativeBuildInputs = [
      ];
      prePnpmInstall = ''
        export SYSTEM=${stdenv.system}
      '';

    }).overrideAttrs
      (old: {
        installPhase = builtins.replaceStrings [ "--force" ] [ "" ] old.installPhase;
      });

  # dsh 的 node-addon-require-builtin 要在进程里用「精确字节匹配」找到 Node 内部的
  # `node::PrincipalRealm::builtin_module_require() const`（一个 this->field getter），
  # 期望的机器码是 `mov rax,[rdi+0x200] ; ret`。
  # nixpkgs 默认开着的 zerocallusedregs 加固（-fzero-call-used-regs=used-gpr）会在
  # mov 和 ret 之间插一条 `xor edi,edi`，匹配器就不认了，启动直接挂在：
  #   dsh: host preparation failed: node-addon-require-builtin unsupported:
  #   Unsupported/no-getter (x64 sysv getter is not a recognized this->field accessor)
  #
  # 关键：必须改「真正编译 node 的那份」= nodejs-slim。
  # pkgs.nodejs 只是把 nodejs-slim 的输出用 lndir 拼起来的包装包（buildCommand 里
  # 就是 lndir，bin/node 是指向 slim 二进制的链接），对它 overrideAttrs 只会换掉
  # 那个拼装 derivation 的 hash，机器码一个字节都不会变，问题依旧。
  nodejsForDsh = nodejs-slim.overrideAttrs (old: {
    hardeningDisable = (old.hardeningDisable or [ ]) ++ [ "zerocallusedregs" ];
    doCheck = false; # 本地编译，跳过 node 自带那套超长测试
  });
in
stdenv.mkDerivation {
  inherit
    pname
    version
    src
    pnpmDeps
    ;
  nativeBuildInputs = [
    nodejsForDsh
    pnpm
    pnpmConfigHook
    pnpmBuildHook
    git
    makeWrapper
  ];
  dontNpmPrune = true;
  dontCheckForBrokenSymlinks = true;

  npmWorkspace = "dsh";

  installPhase = ''
    root=$out/lib/node_modules/@deepseek-ai/dsh-root
    mkdir -p $root
    cp -r * $root

    dsh_package=$out/lib/node_modules/@deepseek-ai/dsh/
    mkdir -p $dsh_package
    ln -s $root/apps/cli/node_modules $dsh_package

    mkdir -p $out/bin
    makeWrapper ${lib.getExe nodejsForDsh} $out/bin/dsh \
      --argv0 dsh \
      --add-flags "--expose-internals" \
      --add-flags "$root/apps/cli/lib/bin.js"
  '';
}
