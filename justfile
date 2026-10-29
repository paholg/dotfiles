set shell := ["/usr/bin/env", "bash", "-euo", "pipefail", "-c"]

[private]
default:
	@just --list

# Update system
up *args: up-git up-host up-fw
	@just sw {{args}}

upr *args: up-git up-host up-fw
	@just swr {{args}}

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

build *args: 
	nh os build . -- --extra-experimental-features 'nix-command flakes' {{args}}

build-remote *args:
	nh os build . --build-host paho@fractal -- --extra-experimental-features 'nix-command flakes' --cores 0 {{args}}
	
# Switch NixOS
sw *args:
	nh os switch . -- --extra-experimental-features 'nix-command flakes' {{args}}

swr *args:
	nh os switch . --build-host paho@fractal -- --extra-experimental-features 'nix-command flakes' --cores 0 {{args}}

# Run an ESPHome command on a device, e.g. `just esphome dining-paper run`
esphome device cmd *args:
	#!/usr/bin/env bash
	set -euo pipefail
	config=$(nix build --no-link --print-out-paths .#nixosConfigurations.box.config.custom.esphome.{{device}})
	set -a
	source <(cd secrets && agenix -d esphome_env)
	set +a
	# Build output would otherwise go next to the config, in the read-only store.
	export ESPHOME_DATA_DIR=~/.cache/esphome
	nix run --inputs-from . nixpkgs#esphome -- {{cmd}} "$config/{{device}}.yaml" {{args}}
