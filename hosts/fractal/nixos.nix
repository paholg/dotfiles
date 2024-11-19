{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];
  system.stateVersion = "23.11";
  networking.hostName = "fractal";

  custom = {
    gui = true;
    ssh = true;
  };

  hardware.xone.enable = true;

  # For display-switch
  hardware.i2c.enable = true;

  networking.networkmanager.enable = false;
  networking.wireless.enable = false;

  users.users.guest = {
    description = "Guest";
    isNormalUser = true;
  };

  # For rebinding mouse
  services.ratbagd.enable = true;

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
