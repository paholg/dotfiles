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
    };

    location.provider = "geoclue2";

    environment.sessionVariables = {
      GTK_DATA_PREFIX = [ "${config.system.path}" ];
    };

    programs.dconf.enable = true;

    fonts.packages = with pkgs; [ (nerdfonts.override { fonts = [ "FiraCode" ]; }) ];

    # Workaround for swaylock not accepting my password.
    security.pam.services.swaylock = { };

    # Pipewire audio
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };
    hardware.pulseaudio.daemon.config = {
      # Fix for Audioengine HD3 speakers
      default-sample-rate = 48000;
    };

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
