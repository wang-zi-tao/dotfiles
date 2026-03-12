---
name: nixpkgs-hash
description: Generate the sha256 hash of nix package or source code repo.
---

# Method 1: Get hash code from error message when building package

## Instructions

1. set hash code to empty string
2. run `./install.bash pkgs <package>` to build package
3. get hash code from error message and update the hash code in `install.bash`
4. edit hash field in the nix file to the new hash code

# which field is hash code?
- sha256

```nix
src = pkgs.fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "rev";
    sha256 = ""; # set to empty string and run install.bash to get the hash code
};
```

- npmDepsHash

```nix

  pkg = pkgs.buildNpmPackage {
    pname = "package name";
    version = "version";

    src = ./.;

    npmDepsHash = ""; # set to empty string and run install.bash to get the hash code
  };
```
