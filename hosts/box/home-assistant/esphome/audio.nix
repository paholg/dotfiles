{ lib, pkgs, ... }:
let
  sendspinZero = import ./sendspin-zero.nix { inherit lib pkgs; };
in
{
  config = lib.mkMerge (
    lib.mapAttrsToList (name: board: sendspinZero (board // { inherit name; })) (import ./audio-boards.nix)
  );
}
