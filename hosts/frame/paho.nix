{ ... }:
{
  imports = [ ../../home ];
  home.stateVersion = "24.05";

  custom = {
    username = "paho";
    gui = true;
    linux = true;
    nixos = true;
    starship.host_color = "cyan";
    xmonad.enable = true;
    # i3.enable = true;
    fish_extra_init =
      # fish
      ''
        set TTY (tty)
        # TTY1: startx
        [ "$TTY" = "/dev/tty1" ] && exec "startx"
      '';
  };
}
