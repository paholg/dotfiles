{ config, pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
    ];

  system.stateVersion = "20.03";

  boot = {
    kernelParams = [
      "consoleblank=60"
    ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking.networkmanager.enable = true;
  networking.useDHCP = false;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  time.timeZone = "America/Los_Angeles";
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  location.provider = "geoclue2";
  nixpkgs.config.allowUnfree = true;

  users.users.paho = {
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
  };

  services = {
    emacs.enable = true;
    physlock = {
      allowAnyUser = true;
      enable = true;
    };
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
      xautolock = {
        enable = true;
        enableNotifier = true;
        locker = ''${config.security.wrapperDir}/physlock'';
        notifier = ''${pkgs.libnotify}/bin/notify-send "Locking in 10 seconds"'';
      };
    };
  };

  environment.systemPackages = with pkgs; [
    acpi
    alacritty
    diffr
    dmenu
    dzen2
    emacs
    exa
    feh
    firefox-bin
    htop
    git
    glibc
    lshw
    pciutils # lspci, etc.
    psmisc # killall, fuser, etc.
    ripgrep
    rust-analyzer
    rustup
    scrot
    xbrightness
    xorg.xmodmap
    xorg.xrandr
    zsh
  ];
}

