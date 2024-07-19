{ config, ... }:
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
    fish_extra_init =
      # fish
      ''
        set TTY (tty)
        # TTY1: startx
        [ "$TTY" = "/dev/tty1" ] && exec "startx"
      '';

    display-switch = {
      enable = true;
      settings = {
        globalSection = {
          usb_device = "046d:c52b";
        };
        sections = {
          monitor1 = {
            monitor_id = "Gigabyte M32U";
            on_usb_connect = "DisplayPort2";
            on_usb_disconnect = "DisplayPort1";
          };
          monitor2 = {
            monitor_id = "HP Z32";
            on_usb_connect = "HDMI1";
            on_usb_disconnect = "DisplayPort1";
          };
        };
      };
    };
  };
}
