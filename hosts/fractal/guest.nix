{ config, ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
  ];
  home.stateVersion = "24.05";

  custom = {
    username = "guest";
    gui = true;
    nixos = true;
    starship.host_color = "cyan";
    xfce.enable = true;
    fishInit = # fish
      ''
        set TTY (tty)
        # TTY1: startx
        [ "$TTY" = "/dev/tty1" ] && exec "startx"
      '';
  };

  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
