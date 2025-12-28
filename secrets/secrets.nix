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
  "authit_config.toml".publicKeys = boxKeys;
  "porkbun_api.json".publicKeys = boxKeys;
  foundry_env.publicKeys = boxKeys;
  oauth2_proxy.publicKeys = boxKeys;
  playlister_env.publicKeys = boxKeys;
  tmdb_api_key.publicKeys = boxKeys;
  vpn_config.publicKeys = boxKeys;
  wireguard_config.publicKeys = boxKeys;
  wireguard_preshared_key.publicKeys = boxKeys;
  wireguard_private_key.publicKeys = boxKeys;
}
