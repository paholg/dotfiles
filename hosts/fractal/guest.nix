{ lib, ... }:
{
  imports = [
    ../../home
  ];
  home.stateVersion = "24.05";

  custom = {
    username = "guest";
    mangohud.enable = false;
    starship.host_color = "cyan";

    fish_extra_init = # fish
      ''
        set TTY (tty)
        # TTY1: startx
        [ "$TTY" = "/dev/tty1" ] && exec "niri-session"
      '';
  };

  programs.niri.settings = {
    outputs = {
      "DP-1" = {
        enable = false;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 138.0;
        };
      };
      "DP-2" = {
        enable = true;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 60.0;
        };
        scale = 2.0;
      };
    };
    spawn-at-startup = lib.mkForce [
      { command = [ "steam" ]; }
    ];
    binds = {
      # Override to disable the locker for guest.
      "Super+Ctrl+N" = lib.mkForce { action.spawn = ""; };
    };
    workspaces = lib.mkForce { };
    window-rules = lib.mkForce [
      {
        open-maximized = true;
      }
    ];
  };
}
