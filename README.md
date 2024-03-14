# dotfiles
Some of my config files and scripts I find useful.

Configuration is now all managed by [home-manager](https://github.com/rycee/
home-manager), so the only configuration here is for that and for NixOS.

# Layout

* `bin/` contains scripts.
* `home/` contains home-manager configuration, which includes all user-level
  config.
* `nix/` contains NixOs configuration.
* `hosts/` contains config for individual hosts.

# Instructions

Create a directory with `hosts/HOSTNAME` as its name, including `home.nix`
for home-manager and/or `configuration.nix` for NixOs. Also add an entry to
`flake.nix`.

TODO: Revisit this, there's probably more to do.
Then, run `nix shell nixpkgs#just -c just install`.
