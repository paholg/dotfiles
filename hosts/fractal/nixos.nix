{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];
  system.stateVersion = "23.11";
  networking.hostName = "fractal";

  custom = {
    gui = true;
    ssh = true;
  };

  # Maybe disable xpadneo for better results?
  # See: https://github.com/ValveSoftware/steam-for-linux/issues/9310#issuecomment-2098573826
  hardware.xpadneo.enable = true;

  # For display-switch
  hardware.i2c.enable = true;

  networking.networkmanager.enable = false;
  networking.wireless.enable = false;

  hardware.graphics = {
    enable = true;

    enable32Bit = true;
    # amdvlk: open-source Vulkan driver from AMD
    extraPackages = [ pkgs.amdvlk ];
    extraPackages32 = [ pkgs.driversi686Linux.amdvlk ];
  };

  users.users.guest = {
    description = "Guest";
    isNormalUser = true;
  };

  services.xserver = {
    enable = true;
    displayManager = {
      # autoLogin.enable = true;
      # autoLogin.user = "guest";
      # gdm.enable = true;
      startx.enable = true;
    };
    # desktopManager.gnome.enable = true;
    desktopManager.xfce.enable = true;
  };
  # systemd.services."getty@tty1".enable = false;
  # systemd.services."autovt@tty1".enable = false;

  programs.steam = {
    enable = true;
    package = pkgs.unfree.steam;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;

    protontricks.enable = true;

    # gamescopeSession = {
    #   enable = true;
    #   env = {
    #     # Games allegedly prefer X11
    #     SDL_VIDEODRIVER = "x11";
    #     # Maybe fix audio stuttering?
    #     PULSE_LATENCY_MSEC = "50";
    #   };
    #   args = [
    #     "-W 3840"
    #     "-H 2160"
    #     # # Maybe help with mouse issues?
    #     # "--force-grab-cursor"
    #     # "--mouse-sensitivity"
    #     # "3"
    #     "--mangoapp"
    #   ];
    # };
  };
}
