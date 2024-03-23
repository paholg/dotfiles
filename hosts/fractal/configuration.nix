{pkgs, ...}: {
  imports = [../../nix/common.nix ../../nix/gui.nix ../../nix/ssh.nix];

  networking.hostName = "fractal";

  # TODO: Figure out if I can get xone working without zen kernel.
  boot.kernelPackages = pkgs.linuxPackages_zen;
  hardware.xone.enable = true;
  environment.systemPackages = [pkgs.linuxKernel.packages.linux_zen.xone];

  networking.networkmanager.enable = false;
  networking.wireless = {
    enable = true;
    networks."A Link to the Past".psk = "011235813";
  };

  hardware.opengl = {
    enable = true;

    # radv: open-source Vulkan driver from freedesktop
    driSupport = true;
    driSupport32Bit = true;

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
        "-e"
        # Maybe help with mouse issues?
        "--force-grab-cursor"
      ];
    };
  };

  services.printing = {
    enable = true;
    drivers = [pkgs.samsung-unified-linux-driver];
  };

  services.getty.autologinUser = "paho";
}
