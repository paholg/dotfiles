{ pkgs, ... }:
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
    display-switch = {
      enable = true;
      settings = {
        globalSection = {
          usb_device = "2109:0817";
        };
        sections = {
          monitor1 = {
            monitor_id = "PG42UQ";
            on_usb_connect = "Hdmi1";
            on_usb_disconnect = "DisplayPort1";
          };
        };
      };
    };
  };

  home.shellAliases = {
    my = "mycli --socket /tmp/mysql.sock -uroot -D scholarly_development";
  };

  home.packages = with pkgs; [
    distrobox
    heroku
    iredis
    mycli
    pscale
  ];
}
