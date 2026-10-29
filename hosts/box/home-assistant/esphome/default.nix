{ lib, ... }:
{
  imports = [ ./dining-paper.nix ];

  # Generated device config directories, by device name. Flash with `just esphome <device> run`.
  options.custom.esphome = lib.mkOption {
    type = lib.types.attrsOf lib.types.package;
    default = { };
  };
}
