{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.custom;
in
{
  options.custom.display-switch = {
    enable = mkEnableOption "Display Switch";

    settings = mkOption {
      type = (pkgs.formats.iniWithGlobalSection { }).type;
      default = { };
      description = "Display Switch config file";
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

  config = mkIf cfg.display-switch.enable {
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
      generators.toINIWithGlobalSection { }
        cfg.display-switch.settings;

    # Requires system blueman service
    services.blueman-applet.enable = true;
  };
}
