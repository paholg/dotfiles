{ ... }:
{
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
          on_usb_disconnect = "Hdmi2";
        };
        monitor2 = {
          monitor_id = "HP Z32";
          # Sadly, this doesn't actually work, as this monitor seems to only
          # accept i2c signals when its already connected. Maybe if I have
          # display-switch with disconnect on both computers?
          on_usb_connect = "DisplayPort1";
          on_usb_disconnect = "Hdmi1";
        };
      };
    };
  };
}
