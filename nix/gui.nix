{ config, pkgs, ... }:

{
  hardware = {
    bluetooth.enable = true;
    pulseaudio = {
      enable = true;
      # Includes bluetooth support:
      package = pkgs.pulseaudioFull;
    };
  };

  location.provider = "geoclue2";
  sound.enable = true;

  environment.sessionVariables = {
    GTK_DATA_PREFIX = [ "${config.system.path}" ];
  };

  services = {
    fwupd.enable = true;

    physlock = {
      allowAnyUser = true;
      enable = true;
    };

    printing = {
      enable = true;
      drivers = [ pkgs.samsungUnifiedLinuxDriver ];
    };

    xserver = {
      enable = true;
      layout = "us";
      libinput = {
        enable = true;
        touchpad = {
          clickMethod = "clickfinger";
          disableWhileTyping = true;
          tapping = false;
        };
      };
      xkbOptions = "eurosign:e";
      desktopManager.plasma5.enable = true;
      displayManager = {
        defaultSession = "plasma5";
        lightdm.enable = true;
      };
      deviceSection = ''
        Option "TearFree" "true"
      '';
      xautolock = {
        enable = true;
        enableNotifier = true;
        locker = "${config.security.wrapperDir}/physlock";
        notifier =
          ''${pkgs.libnotify}/bin/notify-send "Locking in 10 seconds"'';
        time = 10;
      };
    };
  };

  services.avahi.enable = true;

  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
  };
}

