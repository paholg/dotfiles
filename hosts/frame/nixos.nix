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

  # Cap aggregate container memory so a runaway pile of devcontainers gets
  # OOM-killed within the slice instead of thrashing swap and hanging the host.
  virtualisation.docker.daemon.settings."cgroup-parent" = "docker.slice";
  systemd.slices.docker.sliceConfig = {
    MemoryHigh = "16G"; # throttle + reclaim
    MemoryMax = "20G"; # hard cap, scoped OOM-kill
    MemorySwapMax = "4G"; # bound swap to avoid thrash
  };

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

  # Disable Panel Self Refresh to work around DMCUB / flip_done freezes on
  # Phoenix DCN 3.1.4. Trades some idle power for stability.
  # https://community.frame.work/t/dcmub-error-on-bios-3-05-kernel-6-13-1-hit-a-very-nasty-amdgpu-bug-on-framework-laptop-13-amd-ryzen-7-7840u/65371
  boot.kernelParams = [ "amdgpu.dcdebugmask=0x10" ];

  # ****************************************************************************
  # v4l2loopback for OBS virtual camera
  programs.obs-studio = {
    enable = true;
    enableVirtualCamera = true;
  };
}
