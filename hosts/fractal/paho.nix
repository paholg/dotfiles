{ config, ... }:
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
    xmonad.enable = true;
    fish_extra_init = # fish
      ''
        set TTY (tty)
        # TTY2: startx
        [ "$TTY" = "/dev/tty2" ] && exec "startx"
      '';
  };

  # Store dotfiles in a shared location, so guest can access too:
  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
