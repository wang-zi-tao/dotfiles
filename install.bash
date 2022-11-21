#!/usr/bin/env bash
set -e
command=$1
nix=(nix --experimental-features "nix-command flakes")
shift
case $command in
deploy) nix run .\#deploy-rs -- -d --fast-connection true -c ;;
nix-boost) sh <(curl -L https://nixos.org/nix/install) --daemon ;;
home-manager)
	profile=$1
	shift
	result=$(mktemp -d)
	trap "rm $result -r" EXIT
	os_name=$(uname)
	$nix build ".#homeConfigurations.$(uname -m)-${os_name,,}.$profile.activationPackage" --out-link $result/result $@
	bash $result/result/activate
	;;
nix-on-droid)
	profile=$1
	shift
	result=$(mktemp -d)
	trap "rm $result -r" EXIT
	$nix build ".#nixOnDroidConfigurations.$profile.activationPackage" --out-link $result/result $@ --impure
	bash $result/result/activate
	;;
nixos-self)
	nixos-rebuild --flake ".#${HOST}" --target-host root@localhost switch $@ ||
		sudo nixos-rebuild --flake ".#${HOST}" switch $@
	;;
nixos)
	host=$1
	shift
	nixos-rebuild --flake ".#$host" --target-host root@$host switch $@ ||
		nixos-rebuild --flake ".#$host" --target-host root@$host.wg switch $@ ||
		nixos-rebuild --flake ".#$host" --target-host root@$host.wg1 switch $@
	;;
repl)
	SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)
	$nix run $SCRIPT_DIR\#repl
	;;
nix)
	$nix $@
	;;
*)
	echo "unknown subcommand $command"
	false
	;;
esac
