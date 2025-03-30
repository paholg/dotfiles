{ pkgs, ... }:
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

  config = {
    home.packages = [
      locker
      pkgs.nautilus # File-picker used by our desktop portal
      pkgs.xwayland-satellite
    ];

    programs = {
      fuzzel.enable = true;
      swaylock.enable = true;
      waybar = import ./waybar.nix;

      niri = {
        settings = {
          binds = import ./binds.nix;
          environment = {
            DISPLAY = ":0"; # For xwayland
            NIXOS_OZONE_WL = "1";
          };
          input = {
            focus-follows-mouse = {
              enable = true;
              max-scroll-amount = "0%";
            };
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
          screenshot-path = "~/screenshots/%F_%H.%M.%S.png";
          layout = {
            always-center-single-column = true;
            center-focused-column = "never";
            gaps = 4;
            empty-workspace-above-first = true;
          };
          workspaces = {
            main = { };
            zchat = { };
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
            {
              command = [ "xwayland-satellite" ];
            }
          ];
          window-rules = [
            {
              draw-border-with-background = false;
            }
            {
              matches = [
                { app-id = "pavucontrol"; }
                { title = "Extension: (Bitwarden Password Manager)"; }
                { app-id = ".blueman-manager-wrapped"; }
              ];
              open-floating = true;
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
            {
              matches = [
                { app-id = "discord"; }
                { app-id = "steam"; }
                { app-id = "slack"; }
              ];
              open-on-workspace = "zchat";
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
