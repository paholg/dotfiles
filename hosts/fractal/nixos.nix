{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];
  system.stateVersion = "23.11";
  networking.hostName = "fractal";

  # Need to install here for polit rules to be picked up.
  environment.systemPackages = [ pkgs.quickemu ];
  virtualisation.spiceUSBRedirection.enable = true;

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

  programs.steam = {
    enable = true;
    package = pkgs.steam.override {
      # Hack to fix Proton games using the wrong timezone.
      # https://github.com/NixOS/nixpkgs/issues/338266#issuecomment-2419568331
      extraBwrapArgs = [ "--unsetenv TZ" ];
    };
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
