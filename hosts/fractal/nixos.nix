{ lib, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./vt-autopilot.nix
  ];
  system.stateVersion = "23.11";
  networking.hostName = "fractal";

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 65 * 1024;
    }
  ];

  # Need to install here for polit rules to be picked up.
  environment.systemPackages = [ pkgs.quickemu ];
  virtualisation.spiceUSBRedirection.enable = true;

  # HP EX920 (SM2262, fw SVN163) has a broken NVMe FLUSH: ~240ms/fsync.
  # Its volatile write cache is already disabled (WCE=0), so writes are
  # durable on completion and FLUSH is unnecessary — tell the kernel to stop
  # issuing it. ~2000x faster fsync, no durability loss, no throughput cost.
  services.udev.extraRules = ''
    ACTION=="add|change", SUBSYSTEM=="block", ATTRS{model}=="HP SSD EX920 1TB*", ATTR{queue/write_cache}="write through"
  '';
  # Everything behind the TV's USB hub belongs to the guest session only.
  # The Steam udev rules tag controllers `uaccess`, which hands them to
  # whichever session is active; with tty2 active that is paho, whose desktop
  # Steam then grabs the TV controllers and acts on every press at the TV
  # (Big Picture opens, settings change, client restarts). vt-autopilot runs
  # as root and niri gets input via logind, so neither is affected.
  # Must sort after 70-uaccess.rules (adds the tag) and before
  # 73-seat-late.rules (applies the ACL), so it cannot go in extraRules.
  # Matched by USB port: the hub is on bus 3, port 5.1. Moving the TV's cable
  # to another port on the PC breaks this.
  services.udev.packages = [
    (pkgs.writeTextFile {
      name = "tv-input-udev-rules";
      destination = "/etc/udev/rules.d/72-tv-input.rules";
      text = ''
        SUBSYSTEM=="hidraw|input|usb", SUBSYSTEMS=="usb", ATTRS{busnum}=="3", ATTRS{devpath}=="5.1.*", OWNER="guest", MODE="0600", TAG-="uaccess"
      '';
    })
  ];

  # For display-switch
  hardware.i2c.enable = true;

  networking.networkmanager.enable = false;
  networking.wireless.enable = false;

  users.users.guest = {
    description = "Guest";
    isNormalUser = true;
  };

  # Containers
  virtualisation.podman = {
    enable = false;
    dockerCompat = false;
  };
  virtualisation.docker.enable = true;
  users.users.paho.extraGroups = [ "docker" ];

  # Keep exec sessions live when switching.
  systemd.services.docker.restartIfChanged = false;

  # For rebinding mouse
  services.ratbagd.enable = true;

  # Might be causing amdgpu page faults?
  # services.lact.enable = true;
  # hardware.amdgpu.overdrive.enable = true;

  programs.gamemode.enable = true;

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

    gamescopeSession = {
      enable = true;
      # Session-specific; NOT programs.gamescope.args, which would apply to
      # every gamescope invocation (e.g. gamescope-office).
      args = [
        "--adaptive-sync" # VRR support
        "--rt"
        "--hdr-enabled"
        # See /sys/class/drm/card* for output names that gamescope uses.
        "--prefer-output HDMI-A-1"
      ];
      # SteamOS session flags: required for controller navigation of the Steam
      # UI under embedded gamescope. Same set as ChimeraOS's gamescope-session:
      # https://github.com/ChimeraOS/gamescope-session-steam/blob/main/usr/share/gamescope-session-plus/sessions.d/steam
      steamArgs = [
        "-pipewire-dmabuf"
        "-gamepadui"
        "-steamos3"
        "-steampal"
        "-steamdeck"
      ];
      env = {
        STEAM_GAMESCOPE_VRR_SUPPORTED = "1";
        STEAM_GAMESCOPE_HDR_SUPPORTED = "1";
        # wireplumber handles audio device switching.
        STEAM_DISABLE_AUDIO_DEVICE_SWITCHING = "1";
        # Open URLs in Steam's browser; there is no desktop browser here.
        SRT_URLOPEN_PREFER_STEAM = "1";
        # On-screen Steam keyboard.
        QT_IM_MODULE = "steam";
        GTK_IM_MODULE = "Steam";
      };
    };
  };

  # Run the Steam TV session as guest on VT1.
  services.greetd = {
    enable = true;
    settings.default_session = {
      # greetd points the session's stdout/stderr at the VT, so gamescope's
      # log is lost without systemd-cat.
      command = "${lib.getExe' pkgs.systemd "systemd-cat"} -t steam-gamescope steam-gamescope";
      user = "guest";
    };
  };
  # Module default is on-success; be robust to nonzero exits too.
  systemd.services.greetd.serviceConfig.Restart = lib.mkForce "always";
}
