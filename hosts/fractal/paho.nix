{ pkgs, ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
  ];
  home.stateVersion = "24.05";

  custom = {
    username = "paho";
    mangohud.enable = true;
    starship.host_color = "cyan";
    swaylock.color = "220044";
    fish_extra_init = # fish
      ''
        set TTY (tty)
        [ "$TTY" = "/dev/tty2" ] && exec "niri-session"
      '';
  };

  programs.obs-studio.enable = true;

  programs.niri.settings = {
    outputs = {
      "HDMI-A-1" = {
        enable = true;
        position = {
          x = 0;
          y = 0;
        };
        scale = 1.3;
        transform.rotation = 90;
      };
      "DP-1" = {
        enable = true;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 138.0;
        };
      };
      "DP-2" = {
        enable = false;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 60.0;
        };
        scale = 2.0;
      };
    };
    spawn-at-startup = [
      { command = [ "firefox" ]; }
      { command = [ "discord" ]; }
      { command = [ "steam" ]; }
    ];
    workspaces = {
      "00-steam" = {
        name = "steam";
        open-on-output = "DP-1";
      };
      "01-main".open-on-output = "DP-1";
      "02-chat".open-on-output = "HDMI-A-1";
    };
    window-rules = [
      {
        # Float gam
        matches = [ { title = "client"; } ];
        open-floating = true;
      }
    ];
  };

  home.packages = with pkgs; [
    # TODO: broken
    # blender-hip
    discover-overlay
    ffmpeg
  ];
}
