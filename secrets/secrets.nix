with builtins;
let
  lib = (import <nixpkgs> { }).lib;
  keys = lib.flatten (map (attrs: attrValues attrs) (attrValues (import ../keys.nix)));
  secrets = filter (f: f != "secrets.nix") (attrNames (readDir ./.));
in
listToAttrs (
  map (secret: {
    name = secret;
    value = {
      publicKeys = keys;
    };
  }) secrets
)
