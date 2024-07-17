{...}: {
  imports = [../../home];
  home.stateVersion = "20.09";

  custom = {
    gui = true;
    nixos = false;
    starship.host_color = "yellow";
    xmonad.enable = true;
  };

  custom.display-switch = {
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
}
