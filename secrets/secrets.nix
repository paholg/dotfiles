with builtins;
let
  keys = (import ../keys.nix);
  boxKeys = with keys; [
    box.system
    box.paho
  ];
in
{
  gandi.publicKeys = boxKeys;
  vpn_config.publicKeys = boxKeys;
}
