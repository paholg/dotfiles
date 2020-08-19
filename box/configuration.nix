{ config, lib, pkgs, ... }:

{
  imports = [ ../nix/common.nix ./plex.nix ];

  swapDevices = [{
    device = "/swapfile";
    priority = 100;
    size = 16384;
  }];

  fileSystems = {
    "/mnt/storage" = {
      device = "/dev/disk/by-uuid/ce5baea0-d13f-48be-88a1-a8b30b493b5e";
      fsType = "btrfs";
    };
  };
  services.btrfs.autoScrub.enable = true;

  networking.hostName = "box";

  environment.systemPackages = with pkgs; [ docker-compose ];

  virtualisation.docker.enable = true;
  users.users.paho.extraGroups = [ "docker" ];
  networking.firewall.allowedTCPPorts = [ 9091 ];
  systemd.services.transmission = {
    description = "Run transmission inside docker with openvpn";
    wantedBy = [ "multi-user.target" ];
    after = [ "docker.service" ];
    requires = [ "docker.service" ];
    serviceConfig = {
      User = "paho";
      Type = "oneshot";
      RemainAfterExit = true;
      WorkingDirectory = /home/paho/dotfiles/box/torrent;
      ExecStart =
        "${pkgs.docker-compose}/bin/docker-compose up -d --remove-orphans";
      ExecStop = "${pkgs.docker-compose}/bin/docker-compose stop";
    };
  };

  # TODO: Switch to ddclient once > 3.9.1
  services.cron = {
    enable = true;
    systemCronJobs = [
      "*/5 * * * * paho . /etc/profile; /home/paho/dotfiles/box/ddns.sh >> /tmp/cron.log"
    ];
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
  };

  users.users.paho.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHKrH/6MunuWSo4l+YKk1NQUyVi37rBwJUxG71iOlTmP paho@t14s"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJIBrToUdcjz1LJEyKTmHQ9ALQ14yYj8zHxrwoBpxno JuiceSSH"
  ];
}
