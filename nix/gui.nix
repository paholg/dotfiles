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
        clickMethod = "clickfinger";
        disableWhileTyping = true;
        tapping = false;
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

