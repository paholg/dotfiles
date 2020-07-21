# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  system.stateVersion = "20.03";

  boot = {
    # Need latest kernel for ryzen 4750u video card
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "consoleblank=60"
    ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking.hostName = "t14s";
  networking.networkmanager.enable = true;

  networking.useDHCP = false;
  networking.interfaces.enp2s0f0.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "America/Los_Angeles";
  location.provider = "geoclue2";

  # Services
  services.emacs.enable = true;
  services.physlock = {
    allowAnyUser = true;
    enable = true;
  };
  services.redshift = {
    enable = true;
    temperature = {
      day = 5000;
      night = 3300;
    };
  };
  services.xserver = {
    enable = true;
    layout = "us";
    libinput.enable = true;
    xkbOptions = "eurosign:e";
    videoDrivers = ["amdgpu"];
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

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.paho = {
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
  };

  # nix options for derivations to persist garbage collection
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';
  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  nixpkgs.config.allowUnfree = true;

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
    lshw
    nix-direnv
    pciutils # lspci, etc.
    psmisc # killall, fuser, etc.
    ripgrep
    rustup
    scrot
    xbrightness
    xorg.xmodmap
    xorg.xrandr
    zsh
  ];
}

