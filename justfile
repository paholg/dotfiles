set shell := ["/usr/bin/env", "bash", "-euo", "pipefail", "-c"]

[private]
default:
	@just --list

# Update system
up: \
	up-git \
	up-ubuntu \
	up-yay \
	up-yum \
	up-rust \
	up-host \
	up-fw \
	up-nix \
	sw

swother host:
	nixos-rebuild switch --flake . --target-host {{host}} --use-remote-sudo

secret-edit file:
	cd secrets && cp template {{file}} && agenix -e {{file}}

secret-rekey:
	cd secrets && agenix -r

# Update the dotfiles git repo
[private]
up-git:
	@just run git "git pull"

# Update with apt
[private]
up-ubuntu:
	@just run apt "sudo apt update && sudo apt dist-upgrade && sudo apt autoremove"

# Update on Arch with yay
[private]
up-yay:
	@just run yay yay

# Update with yum
[private]
up-yum:
	@just run yum "sudo yum check-update && sudo yum update"

# Update rust
[private]
up-rust:
	@just run rustup "rustup update"

# Run any host-specific updates
[private]
up-host:
	if test -f "hosts/$(hostname)/up"; then "hosts/$(hostname)/up"; fi

# Update nix flake
[private]
up-nix:
	@just run nix "nix flake update"

# Update firmware
[private]
up-fw:
	#!/usr/bin/env bash
	set -euo pipefail
	fwupdmgr refresh || exit 0
	fwupdmgr get-updates || exit 0
	fwupdmgr update

# Switch nixos and home-manager
sw: switch-nix switch-hm

# Switch NixOs
[private]
switch-nix:
	@just run nixos-rebuild "nh os switch . -- --extra-experimental-features 'nix-command flakes'"

# Switch home-manager
[private]
switch-hm:
	@just run home-manager "nh home switch ."

# Run `cmd` if `bin` exists
[private]
run bin cmd:
	if command -v {{bin}}; then {{cmd}}; fi
