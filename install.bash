#!/usr/bin/env bash
set -e
command=$1
sudo() {
	if command -v sudo &>/dev/null; then
		command sudo "$@"
	else
		"$@"
	fi
}
nix() {
	if ! command -v nix &>/dev/null; then
		command sh <(curl -L https://nixos.org/nix/install) --daemon
	fi
	command nix --experimental-features "nix-command flakes" "$@"
}
shift
system=$(nix-instantiate --eval -E '(import <nixpkgs> {}).stdenv.hostPlatform.system')
system="${system%\"}"
system="${system#\"}"
case $command in
deploy) nix run .\#deploy-rs -- -d --fast-connection true -c "$@" ;;
nix-lang-check) nix run 'nixpkgs#statix' check . ;;
nix-lang-fix) nix run 'nixpkgs#statix' fix . ;;
nix-boost) sh <(curl -L https://nixos.org/nix/install) --daemon ;;
repl) nix run "$(realpath "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)")"\#repl ;;
nix) nix "$@" ;;
shell)
	mkdir .direnv || true
	direnv allow .
	;;
pkgs)
	package=$1
	shift
	nix build ".#vars.$system.pkgs.$package" "$@"
	;;
home-manager)
	profile=$1
	shift
	if ! command -v nix &>/dev/null; then
		sh <(curl -L https://nixos.org/nix/install) --daemon
	fi
	nix build ".#homeConfigurations.$system.$profile.activationPackage" "$@"
	bash ./result/activate
	;;
nix-on-droid)
	profile=$1
	shift
	nix build ".#nixOnDroidConfigurations.$profile.activationPackage" --impure "$@"
	bash ./result/activate
	;;
nixos)
	nix build ".#nixos.$(hostname).config.system.build.toplevel" "$@"
	sudo nix-env -p /nix/var/nix/profiles/system --set "$(readlink ./result)"
	sudo ./result/bin/switch-to-configuration switch
	;;
disk)
	profile=$1
	shift
	nix build ".#nixos.$profile.config.system.build.disko" "$@"
	sudo ./result
    ;;
system)
	profile=$1
	shift
    nix build ".#nixos.$profile.config.system.build.toplevel" "$@"
    sudo nix-env -p /nix/var/nix/profiles/system --set "$(readlink ./result)"
    sudo ./result/bin/switch-to-configuration switch
	;;
compile-all)
	nix build ".#all.x86_64-linux" --option binary-caches "" "$@"
	nix run .\#deploy-rs -- -d --fast-connection true -c
	;;
*)
	echo "unknown subcommand $command"
	false
	;;
esac
