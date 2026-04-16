set shell := ["/usr/bin/env", "bash", "-euo", "pipefail", "-c"]

[private]
default:
	@just --list

# Update system
up *args: up-git up-host up-fw
	@just sw {{args}}

swother host:
	nixos-rebuild switch --flake . --target-host {{host}} --use-remote-sudo

secret-edit file:
	cd secrets && cp template {{file}} && agenix -e {{file}}

secret-rekey:
	cd secrets && agenix -r

# Update the dotfiles git repo
[private]
up-git:
	git pull

# Run any host-specific updates
[private]
up-host:
	if test -f "hosts/$(hostname)/up"; then "hosts/$(hostname)/up"; fi

# Update firmware
[private]
up-fw:
	#!/usr/bin/env bash
	set -euo pipefail
	fwupdmgr refresh || exit 0
	fwupdmgr get-updates || exit 0
	fwupdmgr update

# Switch NixOS
sw *args:
	nh os switch . -- --extra-experimental-features 'nix-command flakes' {{args}}

# Switch NixOS, building on fractal
sw-remote *args:
	nh os switch . --build-host paho@fractal -- --extra-experimental-features 'nix-command flakes' --cores 0 {{args}}
