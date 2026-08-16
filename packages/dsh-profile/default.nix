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

    mkdir -p "$out"
    cp -r package.json pnpm-lock.yaml pnpm-workspace.yaml node_modules "$out"/
    ln -s ${cordis_patch_yaml} $out/cordis.patch.yml

    rm -rf "$out/node_modules/@deepseek-ai"
    ln -s ${package}/lib/node_modules/@deepseek-ai/dsh/node_modules/@deepseek-ai \
        "$out/node_modules/"

    node ${./generate-bundles.mjs} "$out/package.json" "$out/node_modules" ${pluginArgs}

    runHook postInstall
  '';

  meta = with lib; {
    description = "Nix-built dsh web profile: pnpm-managed plugins as a read-only store layer";
    platforms = platforms.unix;
  };
}
