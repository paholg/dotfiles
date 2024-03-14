set shell := ["/usr/bin/env", "bash", "-euo", "pipefail", "-c"]

up: up-git up-ubuntu up-yay up-yum up-rust up-host up-nix switch switch-hm


up-git:
	@just _up git "git pull"

up-ubuntu:
	@just _up apt "sudo apt update && sudo apt dist-upgrade"

up-yay:
	@just _up yay yay

up-yum:
	@just _up yum "sudo yum check-update && sudo yum update"

up-rust:
	@just _up rustup "rustup update"

up-host:
	if test -f "hosts/$(hostname)/up"; then "hosts/$(hostname)/up"; fi

up-nix:
	@just _up nix "nix flake update"

up-fw:
	#!/usr/bin/env bash
	if command -v fwupdmgr &> /dev/null; then
	    if fwupdmgr refresh; then
	        if fwupdmgr get-updates; then
	            fwupdmgr update
	        fi
	    fi
	fi


_up bin cmd:
	command -v {{bin}} && {{cmd}} || true

switch: switch-nix switch-hm

switch-nix:
	# TODO: Make pure
	@just _up nixos-rebuild "sudo nixos-rebuild --flake . switch --impure"

switch-hm:
	@just _up home-manager "home-manager --flake . switch"

