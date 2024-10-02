{ config, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./vanta-service.nix
  ];

  system.stateVersion = "23.11";
  networking.hostName = "frame";

  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
  '';
  security.polkit.enable = true;

  custom = {
    gui = true;
    ssh = true;
    amd-graphics = true;
  };

  boot.initrd.luks.devices."luks-0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb".device = "/dev/disk/by-uuid/0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb";

  networking.networkmanager.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';

  virtualisation.podman = {
    enable = true;
    autoPrune.enable = true;
    dockerCompat = true;
    defaultNetwork.settings.dns_enabled = true;
  };

  environment.systemPackages = [ pkgs.vanta-agent ];

  programs.adb.enable = true;
  users.users.paho.extraGroups = [
    "adbusers"
    "kvm"
  ];

  services.mysql = {
    enable = true;
    package = pkgs.mysql80;
  };

  services.redis.servers."" = {
    enable = true;
  };

  services.power-profiles-daemon = {
    enable = true;
  };

  services.vanta-agent = {
    enable = true;
  };
  systemd.tmpfiles.rules = [
    "L /etc/vanta.conf - - - - ${config.age.secrets.vanta.path}"
  ];

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
}
