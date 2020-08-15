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
        clickMethod = "clickfinger";
        disableWhileTyping = true;
        tapping = false;
      };
      xkbOptions = "eurosign:e";
      # desktopManager.plasma5.enable = true;
      # displayManager = {
      #   defaultSession = "none+xmonad";
      #   lightdm.enable = true;
      # };
      deviceSection = ''
        Option "TearFree" "true"
      '';
      # xautolock = {
      #   enable = true;
      #   enableNotifier = true;
      #   locker = "${config.security.wrapperDir}/physlock";
      #   notifier =
      #     ''${pkgs.libnotify}/bin/notify-send "Locking in 10 seconds"'';
      #   time = 15;
      # };
    };
  };

  services.avahi.enable = true;

  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
  };

  environment.systemPackages = with pkgs; [
    adwaita-qt
    alacritty
    arandr
    (chromium.override {
      commandLineArgs = "--load-media-router-component-extension=1";
    })
    dmenu
    dzen2
    firefox-bin
    gimp
    gnome3.eog
    libreoffice
    okular
    pavucontrol
    scrot
    signal-desktop
    vlc
    xbrightness
    xorg.xmodmap
    xorg.xrandr
    xournal
  ];
}

