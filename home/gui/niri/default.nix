{ pkgs, ... }:
{
  imports = [
    ./binds.nix
    ./background.nix
    ./locker.nix
    ./rustybar.nix
  ];

  config = {
    home.packages = [
      pkgs.nautilus # File-picker used by our desktop portal
      pkgs.wl-clipboard-rs
      pkgs.xwayland-satellite
    ];

    programs = {
      fuzzel.enable = true;
      swaylock = {
        enable = true;
        settings = {
          show-failed-attempts = true;
        };
      };

      niri = {
        settings = {
          environment = {
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
              xkb = {
                options = "caps:backspace";
              };
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
          window-rules = [
            {
              matches = [
                { app-id = "Alacritty"; }
                { app-id = "kitty"; }
              ];
              draw-border-with-background = false;
            }
            # Floating
            {
              matches = [
                { app-id = "pavucontrol"; }
                { app-id = ".blueman-manager-wrapped"; }
              ];
              open-floating = true;
            }
            # Chat Workspace
            {
              matches = [
                { app-id = "discord"; }
                { app-id = "steam"; }
                { app-id = "Slack"; }
              ];
              open-on-workspace = "chat";
            }
            {
              matches = [
                { app-id = "Zoom Workplace"; }
                { app-id = "zoom"; }
              ];
              excludes = [
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
        settings.default-timeout = 60000;
      };
    };
  };
}
