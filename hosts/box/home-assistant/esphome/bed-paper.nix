{ lib, pkgs, ... }:
let
  paperS3 = import ./paper-s3.nix { inherit lib pkgs; };
  inherit (import ../helpers.nix { inherit lib; }) act;
  inherit (import ../entities.nix { inherit lib; }) light scene;
in
paperS3 {
  name = "bed-paper";
  friendlyName = "Bedroom Paper";
  address = "10.0.0.11";
  buttons = [
    {
      id = "wake_up";
      label = "Wake Up";
      icon = "mdi:white-balance-sunny";
      action = act "turn_on" scene.morning { };
    }
    {
      id = "sleepy_time";
      label = "Sleepy Time";
      icon = "mdi:weather-night";
      action = act "turn_on" scene.sleepy { };
    }
  ];
  sliders = [
    {
      id = "bed";
      label = "Bed";
      light = light.bed;
    }
    {
      id = "stairs";
      label = "Stairs";
      light = light.stairs;
    }
  ];
}
