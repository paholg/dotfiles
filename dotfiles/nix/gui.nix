{ pkgs, ... }:

{
  hardware = {
    pulseaudio = {
      enable = true;
      # Includes bluetooth support:
      package = pkgs.pulseaudioFull;
    };
    bluetooth.enable = true;
  };

  location.provider = "geoclue2";
  sound.enable = true;

  services = {
    # physlock = {
    #   allowAnyUser = true;
    #   enable = true;
    # };
    redshift = {
      enable = true;
      temperature = {
        day = 5000;
        night = 3300;
      };
    };
    xserver = {
      enable = true;
      layout = "us";
      libinput.enable = true;
      xkbOptions = "eurosign:e";
      displayManager.sddm.enable = true;
      displayManager.defaultSession = "none+xmonad";
      deviceSection = ''
        Option "TearFree" "true"
      '';
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = haskellPackages: [
            haskellPackages.xmonad
            haskellPackages.xmonad-contrib
            haskellPackages.xmonad-extras
          ];
        };
      };
      # xautolock = {
      #   enable = true;
      #   enableNotifier = true;
      #   locker = ''${config.security.wrapperDir}/physlock'';
      #   notifier = ''${pkgs.libnotify}/bin/notify-send "Locking in 10 seconds"'';
      # };
    };
  };

  environment.systemPackages = with pkgs; [
    alacritty
    arandr
    dmenu
    dzen2
    emacs
    firefox-bin
    pavucontrol
    scrot
    xbrightness
    xorg.xmodmap
    xorg.xrandr
  ];
}

