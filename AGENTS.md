This repo contains NixOS and Home Manager configurations using Nix flakes.

## Hosts

- **box** - Home server (nginx, home-assistant, media services, VPN, Foundry VTT)
- **fractal** - Desktop
- **frame** - Laptop

## Structure

- `hosts/` - Per-host NixOS and Home Manager configurations
- `home/` - Shared Home Manager modules
- `nixos/` - Shared NixOS modules
- `secrets/` - Agenix-encrypted secrets

## Commands

Use `just` for common operations:
- `just sw` - Switch NixOS and Home Manager configs locally

## Guidelines

- When diagnosing issues, find proof of the underlying cause from systemd logs
  (`journalctl`, `systemctl status`) before proposing a solution.
- When proposing configuration changes, find documentation for those options
  and provide references (e.g., NixOS options search, Home Manager manual).
- Privileged commands (`sudo`, access to `/mnt/storage`) will fail. Ask me to
  run them and share the output instead.
