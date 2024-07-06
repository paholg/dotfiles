{ ... }:
{
  imports = [ ../../home ];
  home.stateVersion = "20.09";

  custom.home = {
    gui = true;
    nixos = true;
  };
  custom.hyprland.enable = true;
  custom.starship.host_color = "cyan";

  custom.display-switch = {
    enable = true;
    settings = {
      globalSection = {
        usb_device = "046d:c52b";
      };
      sections = {
        monitor1 = {
          monitor_id = "Gigabyte M32U";
          on_usb_connect = "DisplayPort1";
          on_usb_disconnect = "DisplayPort2";
        };
        monitor2 = {
          monitor_id = "HP Z32";
          # Sadly, this doesn't actually work, as this monitor seems to only
          # accept i2c signals when its already connected. Maybe if I have
          # display-switch with disconnect on both computers?
          on_usb_connect = "DisplayPort1";
          on_usb_disconnect = "HDMI1";
        };
      };
    };
  };

  # Requires system blueman service
  services.blueman-applet.enable = true;
}
