{ lib, pkgs, ... }:
let
  # Lock the session, running swayidle on a fast loop to turn off displays while
  # locked.
  locker = pkgs.writeShellApplication {
    name = "locker";
    runtimeInputs = with pkgs; [ swayidle ];
    text = # bash
      ''
        # Create a FD and block on it, while capturing swaylock's PID for later
        # use (unlocking).
        #
        # This should give us the same security as `swaylock -f` while also
        # obtaining the PID.
        exec {ready_fd}<> <(:)
        swaylock --ready-fd $ready_fd &
        echo $! > "$XDG_RUNTIME_DIR/swaylock.pid"
        read -ru $ready_fd
        exec {ready_fd}<&-

        dbus-send --system \
          --print-reply \
          --dest=org.gnome.DisplayManager /org/gnome/DisplayManager/LocalDisplayFactory \
          org.gnome.DisplayManager.LocalDisplayFactory.CreateTransientDisplay
      '';
  };

  idler = pkgs.writeShellApplication {
    name = "idler";
    runtimeInputs = [
      pkgs.swayidle
      locker
    ];
    text = # bash
      ''
        swayidle -w timeout 300 locker
      '';
  };
in
{
  imports = [
    ./binds.nix
    ./background.nix
    ./waybar.nix
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
          spawn-at-startup = [
            {
              command = [
                "systemctl"
                "--user"
                "restart"
                "waybar.service"
              ];
            }
            # { command = [ (lib.getExe idler) ]; }
          ];
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
              matches = [ { app-id = "Zoom Workplace"; } ];
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

    systemd.user.services.idler = {
      Unit = {
        Description = "Lock when idle";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
        Requisite = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = lib.getExe idler;
        Restart = "on-failure";
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
