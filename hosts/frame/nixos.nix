{ lib, pkgs, ... }:
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
  services.logind.settings.Login = {
    HandlePowerKey = "suspend";
  };

  # Need to install here for polit rules to be picked up.
  environment.systemPackages = with pkgs; [
    quickemu
  ];
  services.samba.enable = true;

  virtualisation.podman = {
    # This deletes distrobox images :(
    autoPrune.enable = false;
    defaultNetwork.settings.dns_enabled = true;
  };

  services.mysql = {
    enable = true;
    package = pkgs.mysql80;
    settings = {
      mysqld = {
        expire_logs_days = 7;
        max_binlog_size = "100M";
        # sort_buffer_size = "512k";
      };
    };
  };

  services.power-profiles-daemon.enable = true;

  services.redis.servers."" = {
    enable = true;
  };

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
  boot.kernelPackages = pkgs.linuxPackages_6_6;
}
