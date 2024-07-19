{
  lib,
  pkgs,
  config,
  ...
}:
with lib; let
  cfg = config.custom;
in {
  options.custom = {
    gui = mkEnableOption "Enable gui";
  };

  config = mkIf cfg.gui {
    hardware = {
      bluetooth.enable = true;
      bluetooth.powerOnBoot = true;
    };

    location.provider = "geoclue2";

    environment.sessionVariables = {
      GTK_DATA_PREFIX = ["${config.system.path}"];
    };

    programs.dconf.enable = true;

    # Workaround for swaylock not accepting my password.
    security.pam.services.swaylock = {};

    services = {
      blueman.enable = true;

      gnome = {
        gnome-keyring.enable = true;
      };

      printing = {
        enable = true;
        drivers = [pkgs.samsung-unified-linux-driver];
      };
    };

    qt = {
      enable = true;
      platformTheme = "gnome";
      style = "adwaita-dark";
    };
  };
}
