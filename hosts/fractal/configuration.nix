{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../../nix/common.nix
    ../../nix/gui.nix
    ../../nix/ssh.nix
  ];

  system.stateVersion = "23.11";

  networking.hostName = "fractal";

  # Maybe disable xpadneo for better results?
  # See: https://github.com/ValveSoftware/steam-for-linux/issues/9310#issuecomment-2098573826
  hardware.xpadneo.enable = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  services.blueman.enable = true;

  # For display-switch
  hardware.i2c.enable = true;

  networking.networkmanager.enable = false;
  networking.wireless.enable = false;

  hardware.opengl = {
    enable = true;

    # amdvlk: open-source Vulkan driver from AMD
    extraPackages = [pkgs.amdvlk];
    extraPackages32 = [pkgs.driversi686Linux.amdvlk];
  };

  # Audio setup
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  hardware.pulseaudio.enable = false;
  hardware.pulseaudio.daemon.config = {
    # Fix for Audioengine HD3 speakers
    default-sample-rate = 48000;
  };

  # services.xserver = {
  #   enable = true;
  #   displayManager.autoLogin = {
  #     enable = true;
  #     user = "paho";
  #   };

  #   displayManager.defaultSession = "gnome";
  #   displayManager.gdm.enable = true;
  #   desktopManager.gnome.enable = true;
  #   # displayManager.sddm.enable = true;
  #   # displayManager.defaultSession = "plasmawayland";
  #   # desktopManager.plasma5.enable = true;
  # };

  programs.hyprland.enable = true;

  programs.steam = {
    enable = true;
    package = pkgs.unfree.steam;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    gamescopeSession = {
      enable = true;
      env = {
        # Games allegedly prefer X11
        SDL_VIDEODRIVER = "x11";
        # Maybe fix audio stuttering?
        PULSE_LATENCY_MSEC = "50";
      };
      args = [
        "-W 3840"
        "-H 2160"
        # # Maybe help with mouse issues?
        # "--force-grab-cursor"
        # "--mouse-sensitivity"
        # "3"
        # Maybe causes segfaults?
        # "--mangoapp"
      ];
    };
  };

  services = {
    # Network printer autodiscovery
    # avahi = {
    #   enable = true;
    #   nssmdns4 = true;
    #   openFirewall = true;
    # };
    printing = {
      enable = true;
      drivers = [pkgs.unfree.samsung-unified-linux-driver];
    };
  };

  services.getty.autologinUser = "paho";
}
