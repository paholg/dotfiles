set shell := ["/usr/bin/env", "bash", "-euo", "pipefail", "-c"]

[private]
default:
	@just --list

# Update system
up: \
	up-git \
	up-host \
	up-fw \
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
	git pull


# Update rust
[private]
up-rust:
	rustup update

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

# Show failed or crash-looping services
healthcheck:
	#!/usr/bin/env bash
	set -euo pipefail
	echo "=== Failed/stuck services ==="
	systemctl list-units --type=service --state=failed,auto-restart,deactivating --no-pager
	echo "=== Services with excessive restarts (>10) ==="
	systemctl show --property=Id,NRestarts -- \
		$(systemctl list-units --type=service --no-legend --no-pager | awk '{print $1}') \
		2>/dev/null | \
		awk -F= '/^Id=/{id=$2} /^NRestarts=/{if ($2+0>10) print id": "$2" restarts"}'
	echo "=== Application health checks ==="
	curl -sf --connect-timeout 3 --max-time 5 http://localhost:8099/zigbee/ > /dev/null 2>&1 \
		|| echo "  zigbee2mqtt: frontend not responding (port 8099)"
	echo "=== ZFS pool health ==="
	zpool status -x
	echo "=== Drive health (smartd alerts, past 7 days) ==="
	if systemctl is-active --quiet smartd; then
		alerts=$(journalctl -u smartd --since "7 days ago" --priority=warning --no-pager 2>/dev/null || true)
		if [[ -n "$alerts" ]]; then
			echo "$alerts"
		else
			echo "  No recent drive alerts"
		fi
	else
		echo "  smartd not running"
	fi

# Switch nixos and home-manager
sw: switch-nix switch-hm

# Switch, accepting warnings
force-sw:
	@just run nixos-rebuild "nh os switch . -- --extra-experimental-features 'nix-command flakes'"
	

# Switch NixOs
[private]
switch-nix:
	@NIX_ABORT_ON_WARN=true just force-sw

# Switch home-manager
[private]
switch-hm:
	@just run home-manager "nh home switch ."

# Run `cmd` if `bin` exists
[private]
run bin cmd:
	if command -v {{bin}}; then {{cmd}}; fi
