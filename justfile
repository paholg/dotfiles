set shell := ["/usr/bin/env", "bash", "-euo", "pipefail", "-c"]

# Update system
up: up-git up-ubuntu up-yay up-yum up-rust up-host up-nix sw

# Update the dotfiles git repo
up-git:
	@just run git "git pull"

# Update with apt
up-ubuntu:
	@just run apt "sudo apt update && sudo apt dist-upgrade"

# Update on Arch with yay
up-yay:
	@just run yay yay

# Update with yum
up-yum:
	@just run yum "sudo yum check-update && sudo yum update"

# Update rust
up-rust:
	@just run rustup "rustup update"

# Run any host-specific updates
up-host:
	if test -f "hosts/$(hostname)/up"; then "hosts/$(hostname)/up"; fi

# Update nix flake
up-nix:
	@just run nix "nix flake update"

# Update firmware
up-fw:
	@just run fwupdmgr "if fwupdmgr refresh; then fwupdmgr get-updates && fwupdmgr update; fi"

# Switch nixos and home-manager
sw: switch-nix switch-hm

# Switch NixOs
switch-nix:
	# TODO: Make pure
	@just run nixos-rebuild "sudo nixos-rebuild --flake . switch --impure"

# Switch home-manager
switch-hm:
	@just run home-manager "home-manager --flake . switch"

# Run `cmd` if `bin` exists
[private]
run bin cmd:
	if command -v {{bin}}; then {{cmd}}; fi

# Setup a new host
install:
	@just switch-nix
	if test -f "hosts/$(hostname)/install"; then "hosts/$(hostname)/install"; fi
