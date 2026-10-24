import {readFileSync, writeFileSync} from "node:fs";

const [, , manifestPath, ...extra_bundles] = process.argv;
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));

for (const bundle of extra_bundles) {
    manifest.dsh.profile.bundles.push(bundle);
}

writeFileSync(manifestPath, JSON.stringify(manifest, null, 2) + "\n");

