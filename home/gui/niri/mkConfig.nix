kdlFiles:
{ config, lib, ... }:
let
  paths = map (f: {
    path = f;
    base = builtins.baseNameOf (builtins.toString f);
  }) kdlFiles;
in
{
  programs.niri.config = null;

  xdg.configFile =
    builtins.listToAttrs (
      map (p: {
        name = "niri/${p.base}";
        value.source = p.path;
      }) paths
    )
    // {
      "niri/config.kdl".text = lib.concatMapStringsSep "\n" (p: ''include "${p.base}"'') paths;
    };
}
