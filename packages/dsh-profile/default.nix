{
  lib,
  stdenvNoCC,
  nodejs,
  pnpm,
  pnpmConfigHook,
  fetchPnpmDeps,
  git,
  dsh-hindsight,
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
  cordis_patch_yaml = (formats.yaml { }).generate "cordis.patch.yml" cordis_patch;
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

  buildInputs = plugins;

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

    # 只覆盖 dsh 仓库自己构建出来的 @deepseek-ai 包，保留 profile 从 npm 装进来、
    # 而 dsh 仓库并不产出的那些（例如 @deepseek-ai/dsh-computer-use 与
    # @deepseek-ai/dsh-experimental-computer-use-cua-driver-mcp）。
    # 以前这里整目录 rm -rf，把它们一并删掉，于是 profile 的 Preset 行指向一个
    # 不存在的包，启动时报 “N required plugins did not activate”。
    store_packages=${package}/lib/node_modules/@deepseek-ai/dsh/node_modules/@deepseek-ai
    for p in "$store_packages"/*; do
      rm -rf "$out/lib/node_modules/@deepseek-ai/$(basename "$p")"
    done

    node ${./generate-bundles.mjs} "$out/package.json" "$out/lib/node_modules" ${pluginArgs}

    cp -rsf "$store_packages" \
        "$out/lib/node_modules/"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Nix-built dsh web profile: pnpm-managed plugins as a read-only store layer";
    platforms = platforms.unix;
  };
}
