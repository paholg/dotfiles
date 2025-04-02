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
      pkgs.wl-clipboard-rs
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
            default-column-width = {
              proportion = 0.25;
            };
          };
          switch-events.lid-close.action.spawn = "locker";
          workspaces = {
            "01-main".name = "main";
            "02-chat".name = "chat";
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
              matches = [ { app-id = "Alacritty"; } ];
              draw-border-with-background = false;
            }
            # Firefox
            {
              matches = [ { app-id = "firefox"; } ];
              default-column-width = {
                proportion = 0.5;
              };
            }
            # Floating
            {
              matches = [
                {
                  # TODO: Firefox changes the window name after opening it, so
                  # find a way to make this work.
                  app-id = "firefox";
                  title = "^Extension.*Bitwarden";
                }
                { app-id = "pavucontrol"; }
                { app-id = ".blueman-manager-wrapped"; }
              ];
              open-floating = true;
            }
            # Chat Workspace
            {
              matches = [ { app-id = "discord"; } ];
              open-on-workspace = "chat";
            }
            {
              matches = [
                {
                  app-id = "steam";
                  title = "Steam";
                }
              ];
              open-on-workspace = "chat";
              default-column-width = {
                proportion = 0.5;
              };
            }
            {
              matches = [ { app-id = "Slack"; } ];
              open-on-workspace = "chat";
              default-column-width = {
                proportion = 1.0;
              };
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
