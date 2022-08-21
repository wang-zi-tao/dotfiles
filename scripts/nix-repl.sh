#!/usr/bin/env bash
if [[ -e /etc/nixos/flake.nix ]]; then
	nix run '/etc/nixos#repl'
elif [[ -e ${HOME}/.local/share/nixos/flake.nix ]]; then
	nix run "${HOME}/.local/share/nixos#repl"
fi
