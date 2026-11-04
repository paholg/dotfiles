{ lib, ... }:
{
  imports = [
    ./audio.nix
    ./bed-paper.nix
    ./din-paper.nix
  ];

  # Generated device config directories, by device name. Flash with `just esphome <device>`.
  options.custom.esphome = lib.mkOption {
    type = lib.types.attrsOf lib.types.package;
    default = { };
  };
}
