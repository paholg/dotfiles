{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
let
  cfg = config.custom.gui;
in
{
  options.custom.gui = {
    enable = mkEnableOption "Gui Settings";
  };

  config = mkIf cfg.enable {
    hardware = {
      bluetooth.enable = true;
      bluetooth.powerOnBoot = true;
    };

    location.provider = "geoclue2";
    sound.enable = true;

    environment.sessionVariables = {
      GTK_DATA_PREFIX = [ "${config.system.path}" ];
    };

    services = {
      blueman.enable = true;

      gnome = {
        gnome-keyring.enable = true;
      };

      physlock = {
        allowAnyUser = true;
        enable = true;
      };

      printing = {
        enable = true;
        drivers = [ pkgs.samsung-unified-linux-driver ];
      };

      xserver = {
        enable = true;
        layout = "us";
        displayManager.defaultSession = "gnome";
        displayManager.gdm.enable = true;
        desktopManager.gnome.enable = true;
        # libinput = {
        #   enable = true;
        #   touchpad = {
        #     clickMethod = "clickfinger";
        #     disableWhileTyping = true;
        #     tapping = false;
        #   };
        # };
        # xkbOptions = "eurosign:e";
        # desktopManager.plasma5.enable = true;
        # displayManager = {
        #   defaultSession = "plasma";
        #   lightdm.enable = true;
        # };
        # deviceSection = ''
        #   Option "TearFree" "true"
        # '';
        # xautolock = {
        #   enable = true;
        #   enableNotifier = true;
        #   locker = "${config.security.wrapperDir}/physlock";
        #   notifier = ''${pkgs.libnotify}/bin/notify-send "Locking in 10 seconds"'';
        #   time = 10;
        # };
      };
    };

    qt = {
      enable = true;
      platformTheme = "gnome";
      style = "adwaita-dark";
    };
  };
}
