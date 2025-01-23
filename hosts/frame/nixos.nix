{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
  ];

  system.stateVersion = "23.11";
  networking.hostName = "frame";

  custom = {
    gui = true;
    ssh = true;
  };

  boot.initrd.luks.devices."luks-0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb".device =
    "/dev/disk/by-uuid/0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb";

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

  programs.adb.enable = true;
  users.users.paho.extraGroups = [
    "adbusers"
    "kvm"
  ];

  services.mysql = {
    enable = true;
    package = pkgs.mysql80;
    # settings = {
    #   mysqld.sort_buffer_size = "512k";
    # };
  };

  services.redis.servers."" = {
    enable = true;
  };

  services.power-profiles-daemon = {
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
}
