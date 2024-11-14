{ config, pkgs, ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
  ];
  home.stateVersion = "20.09";

  custom = {
    username = "paho";
    gui = true;
    linux = true;
    mangohud.enable = true;
    nixos = true;
    starship.host_color = "cyan";
    i3.enable = true;
    i3.customConfig = {
      startup = [
        {
          command = "discord";
          always = true;
          notification = true;
        }
        {
          command = "steam";
          always = true;
          notification = true;
        }
        {
          command = "firefox";
          always = true;
          notification = true;
        }
      ];
    };
    fish_extra_init = # fish
      ''
        set TTY (tty)
        [ "$TTY" = "/dev/tty2" ] && exec "startx"
      '';
  };

  home.packages = with pkgs; [
    # blender
    discover-overlay
  ];

  # Store dotfiles in a shared location, so guest can access too:
  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
