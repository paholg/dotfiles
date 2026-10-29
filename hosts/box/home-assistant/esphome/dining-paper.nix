{ lib, pkgs, ... }:
let
  paperS3 = import ./paper-s3.nix { inherit lib pkgs; };
  inherit (import ../helpers.nix { inherit lib; }) act;
  inherit (import ../entities.nix { inherit lib; }) light switch script;
in
paperS3 {
  name = "dining-paper";
  friendlyName = "Dining Room Paper";
  address = "10.0.0.10";
  buttons = [
    {
      id = "espresso";
      label = "Espresso";
      icon = "mdi:coffee";
      state = switch.espresso;
      action = act "toggle" switch.espresso { };
    }
    {
      id = "tv_time";
      label = "TV Time";
      icon = "mdi:movie-open";
      action = act "turn_on" script.tvTime { };
    }
  ];
  sliders = [
    {
      id = "ambient";
      label = "Living Room";
      light = light.livingRoomAmbient;
    }
  ];
}
