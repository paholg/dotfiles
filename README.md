# dotfiles
Some of my config files and scripts I find useful.

Configuration is now all managed by [home-manager](https://github.com/rycee/
home-manager), so the only configuration here is for that and for NixOS.

# Layout

* `bin/` contains scripts.
* `home/` contains home-manager configuration, which includes all user-level
  config.
* `nixos/` contains NixOs configuration.
* `hosts/` contains config for individual hosts.

# Instructions

<!-- 1. Set HOSTNAME -->
<!-- 2. Create `home.nix` and/or `configuration.nix` in hosts/HOSTNAME. -->
<!-- 3. Add entry in flake.nix for HOSTNAME. -->
<!-- 4. Enable experimantal features for "nix-command" and "flakes". -->
<!-- 5. Run ./install.sh -->
<!-- 6. Run `just up` to verify and get any updates. -->

TODO: New instructions/script.
Note for agenix: Need ssh + system ssh key in keys.nix
