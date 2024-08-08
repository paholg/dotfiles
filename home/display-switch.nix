{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom;
in
{
  options.custom.display-switch = {
    enable = lib.mkEnableOption "Display Switch";

    settings = lib.mkOption {
      type = (pkgs.formats.iniWithGlobalSection { }).type;
      default = { };
      description = ''
        Display Switch config file.

        Note: Does not work over DisplayLink.

        Get the USB device using `lsusb`.

        Get the monitor ids using `ddcutil detect`.

        The input IDs are Hdmi1, Hdmi2, DisplayPort2, DisplayPort2. For USB-C,
        it will probably be DisplayPort2.
      '';
      example = {
        globalSection = {
          usb_device = "abcd:1234";
        };
        sections = {
          monitor1 = {
            monitor_id = "Monitor The First";
            on_usb_connect = "DisplayPort1";
            on_usb_disconnect = "DisplayPort2";
          };
          monitor2 = {
            monitor_id = "Monitor The Second";
            on_usb_connect = "HDMI1";
            on_usb_disconnect = "DisplayPort1";
          };
        };
      };
    };
  };

  config = lib.mkIf cfg.display-switch.enable {
    systemd.user.services.display-switch = {
      Unit = {
        Description = "Display switch via USB switch";
      };
      Service = {
        ExecStart = lib.getExe' pkgs.display-switch "display_switch";
        Type = "simple";
        StandardOutput = "journal";
        Restart = "always";
      };
      Install.WantedBy = [ "default.target" ];
    };

    home.file.".config/display-switch/display-switch.ini".text =
      lib.generators.toINIWithGlobalSection { }
        cfg.display-switch.settings;

    # Requires system blueman service
    services.blueman-applet.enable = true;
  };
}
