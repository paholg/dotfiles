{
  config,
  lib,
  pkgs,
  ...
}:
let
  # Lock the session, running swayidle on a fast loop to turn off displays while
  # locked.
  locker = pkgs.writeShellApplication {
    name = "locker";
    runtimeInputs = with pkgs; [
      swayidle
      swaylock
      niri
    ];
    text = # bash
      ''
        swayidle timeout 10 'niri msg action power-off-monitors' resume 'niri msg action power-on-monitors' &
        SWAY_IDLE=$!
        swaylock
        kill $SWAY_IDLE
      '';
  };
in
{
  options.custom.niri = {
    enable = lib.mkEnableOption "niri";
  };

  config = lib.mkIf config.custom.niri.enable {
    custom.wayland = true;

    home.packages = [
      locker
      pkgs.nautilus # File-picker used by our desktop portal
      pkgs.niri
    ];

    home.file.".config/niri/config.kdl".source = ./config.kdl;

    services = {
      gammastep = {
        enable = true;
        latitude = 47.6;
        longitude = -122.3;
        tray = true;
        temperature = {
          day = 6500;
          night = 4500;
        };
      };

      mako = {
        enable = true;
      };
    };

    programs = {
      fuzzel = {
        enable = true;
      };
      swaylock = {
        enable = true;
        settings = {
          color = "003333";
        };
      };
      waybar = import ./waybar.nix;
    };
  };
}
