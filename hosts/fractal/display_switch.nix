{ ... }:
{
  custom.display-switch = {
    enable = true;
    settings = {
      globalSection = {
        usb_device = "2109:0817";
      };
      sections = {
        monitor1 = {
          monitor_id = "PG42UQ";
          on_usb_connect = "DisplayPort1";
          on_usb_disconnect = "Hdmi1";
        };
      };
    };
  };
}
