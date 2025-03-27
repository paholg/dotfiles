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
  imports = [
    ./background.nix
  ];

  options.custom.niri = {
    enable = lib.mkEnableOption "niri";
  };

  config = lib.mkIf config.custom.niri.enable {
    custom.wayland = true;

    home.packages = [
      locker
      pkgs.nautilus # File-picker used by our desktop portal
    ];

    programs = {
      fuzzel.enable = true;
      swaylock.enable = true;
      waybar = import ./waybar.nix;

      niri = {
        settings = {
          binds = import ./binds.nix;
          environment = {
            NIXOS_OZONE_WL = "1";
          };
          input = {
            focus-follows-mouse.enable = true;
            keyboard = {
              repeat-delay = 200;
              repeat-rate = 50;
            };
            touchpad = {
              natural-scroll = false;
              tap = false;
              dwt = true;
              click-method = "clickfinger";
            };
          };
          outputs = {
            "DP-3" = {
              enable = true;
              position = {
                x = 1735;
                y = 0;
              };
            };
            "eDP-1" = {
              enable = true;
              position = {
                x = 0;
                y = 0;
              };
              scale = 1.3;
            };
          };
          screenshot-path = "~/screenshots/%F_%H.%M.%S.png";
          layout = {
            center-focused-column = "never";
            gaps = 4;
          };
          spawn-at-startup = [
            {
              command = [
                "systemctl"
                "--user"
                "restart"
                "waybar.service"
              ];
            }
          ];
          window-rules = [
            {
              draw-border-with-background = false;
            }
            {
              matches = [ { app-id = "Zoom Workplace"; } ];
              excludes = [
                { title = "Zoom - Free Account"; }
                { title = "Zoom - Licensed Account"; }
                { title = "Zoom Meeting"; }
                { title = "Meeting"; }
              ];
              open-floating = true;
              open-focused = false;
            }
          ];
        };
      };
    };

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
  };
}
