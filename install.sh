#!/usr/bin/env -S nix shell --extra-experimental-features 'nix-command flakes' nixpkgs#nh nixpkgs#just --command bash
set -euo pipefail

mkdir -p ~/.config/nix/
echo "experimental-features = nix-command flakes" > ~/.config/nix/nix.conf

just sw

if test -f "hosts/$(hostname)/install"; then
  "hosts/$(hostname)/install"
fi

rm -rf ~/.config/nix
