{ config, lib, pkgs, ... }:

{
  imports = [ ../nix/common.nix ./plex.nix ];

  swapDevices = [{
    device = "/swapfile";
    priority = 100;
    size = 16384;
  }];

  networking.hostName = "box";

  services.btrfs.autoScrub = { enable = true; };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
  };

  users.users.paho.openssh.authorizedKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDhPKjDVaIfGo4hg1xtAYETbI1e5LyL2D5xd9NolCWv9q12ReTynp8Bn2z/t444TYASN6nQmscjjI8AaQxEQYExQ3wxMc5ZrLHhAyXupWgL4w+8OwDmw3OqH0+hEhPlJK7ODGhrDNOA96ZIZz4BrpOn0D9IaaYJ75CJV2JdB/rWUrhenKPv19J5A3ksbL64P4Yf58eAd6kDYhSsYxOi654V+2xND7NxX/qf+3bHSizBSZFTwEMkOvJl+UdqT5aWc7la91D4e3xsIhLA6BFaAs3PXmbDhGVrXQkRYhXzTx+PXpcOvW+IFD4nByOxF4tbrrmTW2VFLfzl+fUDVstq5+LAcXk8Z4MvN+2kGob2mqI5RL0ISmDz0n7RiU3E1oYR9TlYKCBnaTSyumFdAB2Rpyek12T9GvEnz5OdHltQUjQ3QO9DUbSWwEuamIKFMKQFHZveUrdQjeulie9J1ISe8d8GfoqB3xt14T0/qoNsiRPa9CUFF1vD2z8M19g5nGhLFoU= paho@t14s"
  ];
}
