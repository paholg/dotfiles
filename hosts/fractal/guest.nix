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
        enable = true;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 138.0;
        };
        variable-refresh-rate = "on-demand";
      };
      "DP-2" = {
        enable = false;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 120.0;
        };
        scale = 2.0;
        variable-refresh-rate = "on-demand";
      };
    };
    spawn-at-startup = [
      { command = [ "steam" ]; }
    ];
    workspaces = {
      "01-main".open-on-output = "DP-1";
    };

    binds = {
      # Override to disable the locker for guest.
      "Super+Ctrl+N" = lib.mkForce { action.spawn = ""; };
    };

    window-rules = lib.mkForce [ ];
  };
}
