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

# Run an OpenTofu command against the router, e.g. `just router plan`
router *args:
	#!/usr/bin/env bash
	set -euo pipefail

	set -a
	source <(cd secrets && agenix -d router_env)
	set +a
	
	export TF_ENCRYPTION="
	key_provider \"pbkdf2\" \"router\" {
	  passphrase = \"$ROUTER_STATE_PASSPHRASE\"
	}
	method \"aes_gcm\" \"router\" {
	  keys = key_provider.pbkdf2.router
	}
	state {
	  method = method.aes_gcm.router
	  enforced = true
	}
	plan {
	  method = method.aes_gcm.router
	  enforced = true
	}
	"
	# Parallel creates would scramble firewall rule order.
	export TF_CLI_ARGS_apply=-parallelism=1
	nix build -o router/config.tf.json .#router-tf
	nix run .#router-tofu -- --chdir=router init -upgrade -input=false
	nix run .#router-tofu -- -chdir=router {{args}}

# Compile and flash an ESPHome device, then show its logs; picks a device if none is given
esphome device="":
	#!/usr/bin/env bash
	set -euo pipefail
	device={{device}}
	if [[ -z $device ]]; then
		device=$(nix eval --raw .#nixosConfigurations.box.config.custom.esphome \
			--apply 'd: builtins.concatStringsSep "\n" (builtins.attrNames d)' | fzf --height ~40%)
	fi
	config=$(nix build --no-link --print-out-paths ".#nixosConfigurations.box.config.custom.esphome.$device")
	set -a
	source <(cd secrets && agenix -d esphome_env)
	set +a
	# Build output would otherwise go next to the config, in the read-only store.
	export ESPHOME_DATA_DIR=~/.cache/esphome
	# ESP-IDF uses whichever cmake and ninja are on the path.
	PATH=$(nix shell --inputs-from . nixpkgs#cmake nixpkgs#ninja .#esphome -c printenv PATH)
	mac=$(nix eval --raw --file router/hosts.nix "$device.mac")
	# USB if this board is plugged in, otherwise over the air.
	port() {
		local ports=(/dev/serial/by-id/usb-Espressif_*_"$mac"-if00)
		[[ -e ${ports[0]} ]] && echo "${ports[0]}" || echo OTA
	}
	esphome compile "$config/$device.yaml"
	# A blank ESP32-S3 resets every few seconds, taking its serial port with it.
	attempts=20
	until esphome upload "$config/$device.yaml" --device "$(port)"; do
		((--attempts)) || exit 1
		sleep 0.1
	done
	esphome logs "$config/$device.yaml" --device "$(port)"
