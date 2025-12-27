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
  "porkbun_api.json".publicKeys = boxKeys;
  playlister_env.publicKeys = boxKeys;
  foundry_env.publicKeys = boxKeys;
  vpn_config.publicKeys = boxKeys;
  wireguard_config.publicKeys = boxKeys;
  wireguard_private_key.publicKeys = boxKeys;
  wireguard_preshared_key.publicKeys = boxKeys;
  oauth2_proxy.publicKeys = boxKeys;
  tmdb_api_key.publicKeys = boxKeys;
}
