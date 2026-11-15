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

# Switch NixOS, substituting locally and building only what's left on fractal
swb *args:
	nh os switch . -- --extra-experimental-features 'nix-command flakes' \
		--builders 'ssh-ng://fractal-home x86_64-linux - 16 2 big-parallel,kvm,nixos-test,benchmark' \
		--builders-use-substitutes {{args}}

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

	# Cleanup
	trap 'rm -rf router/.terraform router/.terraform.lock.hcl router/config.tf.json' EXIT
	
	nix build -o router/config.tf.json .#router-tf
	nix run .#router-tofu -- --chdir=router init -input=false
	nix run .#router-tofu -- -chdir=router {{args}}

# Compile and flash an ESPHome device, then show its logs; picks a device if none is given
esphome device="":
	bin/esp {{device}}

# Show an ESPHome device's logs, without flashing
esphome-logs device="":
	bin/esp --logs {{device}}

esp-mac:
	nix run nixpkgs#esptool -- read-mac
