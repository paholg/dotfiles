{ pkgs, ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
    (import ../../home/gui/niri/mkConfig.nix [
      "base.kdl"
      "paho.kdl"
      "binds.kdl"
      "fractal.kdl"
    ])
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
    devconcurrent = {
      primaryProject = "devconcurrent";
      projects = {
        devconcurrent.path = "~/src/devconcurrent";
        cookit = {
          path = "~/src/cookit";
          devcontainer = {
            remoteEnv.KITTY_PID = "\${localEnv:KITTY_PID}";
            mounts = [
              {
                type = "bind";
                source = "/run/user/1000/mark-urgent.sock";
                target = "/run/user/1000/mark-urgent.sock";
              }
            ];
          };
        };
        models.path = "~/src/models";
        dcex.path = "~/src/dcex";
      };
    };
  };

  programs.obs-studio.enable = true;

  home.packages = with pkgs; [
    discover-overlay
    ffmpeg
    obsidian
  ];
}
