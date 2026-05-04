{
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
  ];

  age.secrets."frame_shell_init.sh" = {
    file = ../../secrets/frame_shell_init.sh;
    owner = "paho";
  };

  system.stateVersion = "23.11";
  networking.hostName = "frame";

  boot.initrd.luks.devices."luks-0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb".device =
    "/dev/disk/by-uuid/0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb";

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.powersave = false;

  boot.extraModprobeConfig = ''
    options btusb enable_autosuspend=n
  '';

  services.pipewire.wireplumber.extraConfig."10-bluez" = {
    "monitor.bluez.properties" = {
      "bluez5.enable-sbc-xq" = true;
      "bluez5.enable-msbc" = true;
      "bluez5.enable-hw-volume" = true;
      "bluez5.hfphsp-backend" = "native";
    };
  };

  services.logind.settings.Login = {
    HandlePowerKey = "suspend";
    HandleLidSwitchExternalPower = "ignore";
  };

  # Need to install here for polit rules to be picked up.
  environment.systemPackages = with pkgs; [
    quickemu
  ];
  services.samba.enable = true;

  virtualisation.podman = {
    enable = false;
    dockerCompat = false;
  };

  virtualisation.docker.enable = true;
  users.users.paho.extraGroups = [ "docker" ];

  services.flatpak.enable = true;

  services.power-profiles-daemon.enable = true;

  # Allow any USB device to wake from suspend.
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", DRIVERS=="usb", ATTR{power/wakeup}="enabled"
  '';

  # ****************************************************************************
  # Fingerprint
  services.fprintd = {
    enable = true;
  };

  # Start the driver at boot
  systemd.services.fprintd = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig.type = "simple";
  };

  # ****************************************************************************
  # Vanta
  systemd.services.vanta_manager = {
    description = "Start vanta container";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${lib.getExe pkgs.podman} start vanta";
      ExecStop = "${lib.getExe pkgs.podman} stop vanta";
    };
  };

  # ****************************************************************************
  # Seeing display freezes; testing settings to fix.
  # https://lore.kernel.org/amd-gfx/20260422162956.620362-1-sunpeng.li@amd.com/
  # nixpkgs linuxPackages_testing currently ships 7.0; the patch targets 7.1-rc1
  # so override the source until nixpkgs catches up.
  boot.kernelPackages = pkgs.linuxPackagesFor (pkgs.linux_testing.override {
    argsOverride = rec {
      version = "7.1-rc1";
      modDirVersion = "7.1.0-rc1";
      src = pkgs.fetchurl {
        url = "https://git.kernel.org/torvalds/t/linux-${version}.tar.gz";
        hash = "sha256-G1twFrk0o91FgHOk97yLCkkJIDzyB9sfvInLUiZjB+4=";
      };
      # Some options in nixpkgs' common-config no longer exist upstream (AX25,
      # DMABUF_MOVE_NOTIFY, HAMRADIO, etc). Skip the strict check.
      ignoreConfigErrors = true;
    };
  });
  boot.kernelPatches = [
    {
      name = "amd-cursor-vblank-fix";
      patch = ./amd-cursor-vblank-fix.patch;
    }
  ];

  # ****************************************************************************
  # v4l2loopback for OBS virtual camera
  programs.obs-studio = {
    enable = true;
    enableVirtualCamera = true;
  };
}
