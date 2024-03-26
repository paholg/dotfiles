{
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../../home/common.nix
    ../../home/common-linux.nix
    ../../home/gui.nix
    # ../../home/firefox.nix
    ../../home/packages-gui.nix
    ../../home/packages-gui-linux.nix
    ./hyprland.nix
    ../../home/display-switch.nix
  ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
  };

  home.file = {
    ".config/display-switch/display-switch.ini".text = lib.generators.toINIWithGlobalSection {} {
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

  programs.git.userEmail = "paho@paholg.com";

  home.packages = with pkgs; [
    firefox
  ];
}
