#!/usr/bin/env bash
set -e
command=$1
script_dir=$(realpath "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)")
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
	nixversion=$(command nix --version)
	nixversion="${nixversion:12:2}"
	if [[ $nixversion -lt 4 ]]; then
		command nix-shell -p nixFlakes --command "nix --experimental-features 'nix-command flakes' $*"
	else
		echo nix --experimental-features "nix-command flakes" "$@"
		command nix --experimental-features "nix-command flakes" "$@"
	fi
}
shift
system=$(nix-instantiate --eval -E '(import <nixpkgs> {}).stdenv.hostPlatform.system')
system="${system%\"}"
system="${system#\"}"
case $command in
deploy)
	nix build "$script_dir#all.$system" --option binary-caches "" "$@"
	nix run "$script_dir#deploy-rs" -- -d --fast-connection true -c "$@"
	;;
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
	nix build "$script_dir#vars.$system.pkgs.$package" "$@"
	;;
pkgs-shell)
	package=$1
	shift
	nix shell "$script_dir#vars.$system.pkgs.$package" "$@"
	;;
home-manager)
	profile=$1
	shift
	if ! command -v nix &>/dev/null; then
		sh <(curl -L https://nixos.org/nix/install) --daemon
	fi
	nix build "$script_dir#homeConfigurations.$system.$profile.activationPackage" "$@"
	bash ./result/activate
	;;
nix-on-droid)
	profile=$1
	shift
	nix build "$script_dir#nixOnDroidConfigurations.$profile.activationPackage" --impure "$@"
	bash ./result/activate
	;;
nixos)
	nix build "$script_dir#nixos.$(hostname).config.system.build.toplevel" "$@"
	sudo nix-env -p /nix/var/nix/profiles/system --set "$(readlink ./result)"
	sudo ./result/bin/switch-to-configuration switch
	;;
disk)
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.disko" "$@"
	sudo ./result
	;;
system)
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.toplevel" "$@"
	# sudo nix-env -p /nix/var/nix/profiles/system --set "$(readlink ./result)"
	# sudo ./result/bin/switch-to-configuration switch
	;;
compile-all)
	nix build "$script_dir#all.x86_64-linux" --option binary-caches "" "$@"
	nix run "$script_dir#deploy-rs" -- -d --fast-connection true -c
	;;
update)
	nix flake update
	;;
vm)
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.vm" "$@"
	;;
run-vm)
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.vm" "$@"
	NIX_DISK_IMAGE=$(readlink -f "${NIX_DISK_IMAGE:-${HOME}/Temp/$profile.qcow2}") "./result/bin/run-$profile-vm"
	;;
*)
	echo "unknown subcommand $command"
	false
	;;
esac
