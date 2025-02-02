#!/usr/bin/env bash
set -e
command=$1

if [[ $OSTYPE == 'darwin'* ]]; then
	if ! command -v brew &>/dev/null; then
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	fi
	# if ! command -v git &>/dev/null; then
	#     brew install git
	# fi
fi

if ! command -v nix &>/dev/null; then
	if [[ $OSTYPE == 'darwin'* ]]; then
		command sh <(curl -L https://nixos.org/nix/install)
	else
		command sh <(curl -L https://nixos.org/nix/install) --daemon
	fi
fi

script_dir=$(realpath "$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)")
sudo() {
	if [[ $USER == root ]]; then
		"$@"
	elif  command -v sudo &>/dev/null; then
		command sudo "$@"
	else
		"$@"
	fi
}
nix() {
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
	nix run "$script_dir#deploy-rs" -- -d --skip-checks -c "$@"
	;;
nix-lang-check) nix run 'nixpkgs#statix' check . ;;
nix-lang-fix) nix run 'nixpkgs#statix' fix . ;;
nix-boost) sh <(curl -L https://nixos.org/nix/install) --daemon ;;
repl) nix repl --extra-experimental-features 'repl-flake repl' "${script_dir}#vars.$system" ;;
nix) nix "$@" ;;
shell)
	mkdir .direnv || true
	direnv allow .
	;;
develop)
	nix develop
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
pkgs-run)
	package=$1
	shift
	nix run "$script_dir#vars.$system.pkgs.$package" "$@"
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
iso)
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.isoImage" "$@"
	;;
nixos)
	nix build "$script_dir#nixos.$(cat /etc/hostname).config.system.build.toplevel" "$@"
	sudo nix-env -p /nix/var/nix/profiles/system --set "$(readlink ./result)"
	sudo ./result/bin/switch-to-configuration switch
	;;
nixos-wsl)
    set -xe
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.toplevel" "$@"
	sudo nix-env -p /nix/var/nix/profiles/system --set "$(readlink ./result)"
	sudo bash ./result/activate
	;;
nixos-remote)
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.toplevel" "$@"
	result=$(realpath ./result)
	nix copy --to "ssh://root@$profile.wg" "$result"
	ssh "root@$profile.wg" "nix-env -p /nix/var/nix/profiles/system --set $result"
	ssh "root@$profile.wg" "$result/bin/switch-to-configuration switch"
	;;
nixos-remote-to)
	profile=$1
	shift
	ip=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.toplevel" "$@"
	result=$(realpath ./result)
	nix copy --to "ssh://root@$ip" "$result"
	ssh "root@$ip" "nix-env -p /nix/var/nix/profiles/system --set $result"
	ssh "root@$ip" "$result/bin/switch-to-configuration switch"
	;;
nixos-install)
	mount /dev/nvme0n1p7 /mnt
	mkdir /mnt/boot/efi
	mount /dev/nvme0n1p1 /mnt/boot/efi
	profile=$1
	shift
	nix build "$script_dir#nixos.$profile.config.system.build.toplevel" "$@"
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
	nix run "$script_dir#deploy-rs" -- -d --fast-connection true -c --skip-checks
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
