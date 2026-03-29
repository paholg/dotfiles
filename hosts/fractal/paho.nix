{ pkgs, ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
    (import ../../home/gui/niri/mkConfig.nix [
      ../../home/gui/niri/base.kdl
      ../../home/gui/niri/paho.kdl
      ../../home/gui/niri/binds.kdl
      ./niri.kdl
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
  };

  programs.obs-studio.enable = true;

  home.packages = with pkgs; [
    # TODO: broken
    # blender-hip
    discover-overlay
    ffmpeg
  ];
}
