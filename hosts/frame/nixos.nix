{ lib, pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  system.stateVersion = "23.11";
  networking.hostName = "frame";

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

  systemd.services.distrobox-vanta = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
    after = [ "podman.service" ];
    bindsTo = [ "podman.service" ];

    serviceConfig = {
      ExecStart = "${lib.getExe pkgs.podman} start -a vanta";
    };
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
}
