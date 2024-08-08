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

  # Audio setup
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # hardware.pulseaudio.enable = false;
  # hardware.pulseaudio.daemon.config = {
  #   # Fix for Audioengine HD3 speakers
  #   default-sample-rate = 48000;
  # };

  users.users.guest = {
    description = "Guest";
    isNormalUser = true;
  };

  # Auto-login guest on TTY1
  # systemd.targets."autologin-tty1" = {
  #   requires = [ "multi-user.target" ];
  #   after = [ "multi-user.target" ];
  #   unitConfig = {
  #     AllowIsolate = "yes";
  #   };
  # };
  systemd.services."getty@tty1" = {
    description = "Autologin guest on TTY1";
    after = [ "systemd-logind.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = [
        ""
        "@${pkgs.utillinux}/sbin/agetty agetty --login-program ${pkgs.shadow}/bin/login --autologin guest --noclear %I $TERM"
      ];
      Restart = "always";
      Type = "idle";
    };
  };
  # TODO: switch to this when it can be for just one tty
  # https://github.com/NixOS/nixpkgs/issues/81552
  # services.getty.autologinUser = "guest";

  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
    # # windowManager.xmonad = {
    # #   enable = true;
    # #   enableContribAndExtras = true;
    # # };
    desktopManager.xfce.enable = true;
  };

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
