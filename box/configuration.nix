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

  networking.hostName = "box";

  services.btrfs.autoScrub.enable = true;

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
