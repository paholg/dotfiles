names:
{ symlink, lib, ... }:
{
  xdg.configFile =
    builtins.listToAttrs (
      map (name: {
        name = "niri/${name}";
        value.source = symlink "niri/${name}";
      }) names
    )
    // {
      "niri/config.kdl".text = lib.concatMapStringsSep "\n" (name: ''include "${name}"'') names;
    };
}
