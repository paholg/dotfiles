#!/usr/bin/env -S nix shell --extra-experimental-features 'nix-command flakes' nixpkgs#nh nixpkgs#just nixpkgs#git --command bash
set -euo pipefail

# TODO: Rewrite

# just sw

# if test -f "hosts/$(hostname)/install"; then
#   "hosts/$(hostname)/install"
# fi
