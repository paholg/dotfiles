{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.custom;
in
{
  config = lib.mkIf cfg.gui {
    hardware = {
      bluetooth.enable = true;
      bluetooth.powerOnBoot = true;
      graphics.enable = true;
      graphics.enable32Bit = true;
    };

    location.provider = "geoclue2";

    environment.sessionVariables = {
      GTK_DATA_PREFIX = [ "${config.system.path}" ];
    };

    programs.dconf.enable = true;

    fonts.packages = [ pkgs.nerd-fonts.fira-code ];

    # Workaround for swaylock not accepting my password.
    security.pam.services.swaylock = { };

    # Pipewire audio
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      # extraConfig.pipewire = {
      #   "10-clock-rate" = {
      #     "context.properties" = {
      #       # Fix for Audioengine HD3 speakers???
      #       "default.clock.rate" = 48000;
      #       "default.clock.allowed-rates" = [
      #         48000
      #       ];
      #     };
      #   };
      # };
    };

    hardware.printers.ensurePrinters = [
      {
        name = "Samsung_Xpress";
        location = "Paho Office";
        deviceUri = "ipp://10.0.0.2/ipp";
        model = "samsung/M267x.ppd";
      }
    ];

    services = {
      blueman.enable = true;

      gnome = {
        gnome-keyring.enable = true;
      };

      libinput = {
        enable = true;
        touchpad = {
          tapping = false;
          clickMethod = "clickfinger";
          disableWhileTyping = true;
        };
      };

      printing = {
        enable = true;
        drivers = [ pkgs.samsung-unified-linux-driver ];
      };

      xserver = {
        enable = true;
        displayManager.startx.enable = true;
        autorun = false;
        xkb.layout = "us";
      };
    };

    qt = {
      enable = true;
      platformTheme = "gnome";
      style = "adwaita-dark";
    };

  };
}
