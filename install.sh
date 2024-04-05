#!/usr/bin/env -S nix shell nixpkgs#nh nixpkgs#just --command bash
set -euo pipefail

just sw

if test -f "hosts/$(hostname)/install"; then
  "hosts/$(hostname)/install"
fi
