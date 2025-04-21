with builtins;
let
  keys = (import ../keys.nix);
  # allKeys = concatLists (attrValues (mapAttrs (name: value: attrValues value) keys));
  boxKeys = with keys; [
    box.system
    box.paho
  ];
in
{
  gandi.publicKeys = boxKeys;
  vpn_config.publicKeys = boxKeys;
  playlister_env.publicKeys = boxKeys;
}
