import {cpSync, readFileSync, rmSync, writeFileSync} from "node:fs";
import {join} from "node:path";

const [, , manifestPath, modulesDir, ...pluginPaths] = process.argv;
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const dependencies = manifest.dependencies ?? {};

// Additional plugins are passed as store paths after the manifest and
// node_modules arguments. Each path must be a package root containing a
// package.json; we copy it into node_modules/<name> and register it as a
// dependency so the bundle scan below can pick up its dsh.bundle.patch.
for (const packagePath of pluginPaths) {
  const pluginPath = join(packagePath, "lib")
  const pluginManifestPath = join(pluginPath, "package.json");
  const pluginManifest = JSON.parse(readFileSync(pluginManifestPath, "utf8"));
  const packageName = pluginManifest.name;

  if (typeof packageName !== "string" || packageName.trim() === "") {
    throw new Error("Plugin at " + pluginPath + " is missing a package name");
  }

  const version = "file:" + pluginPath;
  const target = join(modulesDir, packageName);

  // Recreate the target so cpSync always materializes the plugin at target
  // rather than nesting it when the target directory already exists.
  rmSync(target, {recursive: true, force: true});
  cpSync(pluginPath, target, {recursive: true});

  dependencies[packageName] = version;
}

manifest.dependencies = dependencies;

// In-box bundles are installation-owned and are not profile dependencies;
// keep them first, then append every installed dependency that declares a
// dsh.bundle patch, in dependency order.
const bundles = [
  "@deepseek-ai/dsh-base",
  ...(manifest.dsh?.profile?.bundles || [])
];

for (const packageName of Object.keys(manifest.dependencies ?? {})) {
  const packageManifestPath = join(modulesDir, packageName, "package.json");
  try {
    const packageManifest = JSON.parse(readFileSync(packageManifestPath, "utf8"));
    if (packageManifest.dsh?.bundle?.patch !== undefined) {
      bundles.push(packageName);
    }
  } catch {
    // Not resolvable as a bundle; leave it out (mirrors dsh plugin reconcile).
  }
}

manifest.dsh = {
  ...manifest.dsh,
  profile: {
    ...manifest.dsh?.profile,
    bundles,
  },
};

writeFileSync(manifestPath, JSON.stringify(manifest, null, 2) + "\n");
