{ pkgs, ... }:
{
  system.stateVersion = "23.11";
  networking.hostName = "fractal";

  custom = {
    gui.enable = true;
    ssh.enable = true;
  };

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  hardware.cpu.intel.updateMicrocode = true;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/1543df69-6898-484e-ad8a-3aa78f46f039";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/9F39-7573";
      fsType = "vfat";
    };

    "/mnt/steam" = {
      device = "/dev/disk/by-uuid/558f52a1-1444-4f6a-a8b9-e30c9d5b16f2";
      fsType = "ext4";
    };
  };

  swapDevices = [ ];

  # Maybe disable xpadneo for better results?
  # See: https://github.com/ValveSoftware/steam-for-linux/issues/9310#issuecomment-2098573826
  hardware.xpadneo.enable = true;

  # For display-switch
  hardware.i2c.enable = true;

  networking.networkmanager.enable = false;
  networking.wireless.enable = false;

  hardware.opengl = {
    enable = true;

    # amdvlk: open-source Vulkan driver from AMD
    extraPackages = [ pkgs.amdvlk ];
    extraPackages32 = [ pkgs.driversi686Linux.amdvlk ];
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

  users.users.guest = {
    description = "Guest";
    isNormalUser = true;
  };
  # Auto-login guest on tty2, where we'll also auto-launch sway.
  systemd.services."getty@tty2" = {
    serviceConfig = {
      Type = "simple";
      ExecStart = [
        ""
        "${pkgs.util-linux}/sbin/agetty agetty -o '-p -f -- \\u' --noclear --autologin guest %I $TERM"
      ];
    };
  };

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
}
