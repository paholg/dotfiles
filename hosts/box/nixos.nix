{ config, lib, ... }:
let
  cfg = config.custom;
in
{
  imports = [
    ./hardware-configuration.nix
    ./media.nix
    ./vpn.nix
    # TODO: Get working
    # ./wireguard.nix
  ];

  options.custom = {
    ips = lib.mkOption { type = lib.types.attrsOf lib.types.str; };
    ports = lib.mkOption { type = lib.types.attrsOf lib.types.int; };
    groups = lib.mkOption { type = lib.types.attrsOf lib.types.int; };
    drives = lib.mkOption { type = lib.types.attrsOf lib.types.str; };
  };

  config = {
    system.stateVersion = "20.03";
    networking.hostName = "box";

    custom = {
      # Setting up NextDNS breaks routing for the nixos-container.
      # Rather than solve this, we just mark it as false since we're using it on
      # our router anyway.
      next_dns = false;
      ssh = true;
      gui = false;

      ips = {
        host = "10.233.1.1";
        container = "10.233.1.2";
      };
      ports = {
        transmission = 9091;
        prowlarr = 9696;
        radarr = 7878;
        sonarr = 8989;
        jellyfin = 8096;
      };
      groups = {
        media = 1100;
      };
      drives = {
        storage = "/mnt/storage";
      };
    };

    fileSystems = {
      "${cfg.drives.storage}" = {
        device = "/dev/disk/by-uuid/ce5baea0-d13f-48be-88a1-a8b30b493b5e";
        fsType = "btrfs";
      };
    };
    services.btrfs.autoScrub.enable = true;

    # ****************************************************************************
    # ZFS STUFF
    boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    services.zfs.autoScrub.enable = true;
    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.forceImportRoot = false;
    # ZFS Tuning taken from https://jrs-s.net/2018/08/17/zfs-tuning-cheat-sheet/
    boot.extraModprobeConfig = ''
      options zfs ashift=12 xattr=sa compression=lz4 atime=off recordsize=1M
    '';
    networking.hostId = "b0c5b0c5";
    # TODO:
    # boot.zfs.extraPools = ["storage"];
    # ****************************************************************************

    services.cron = {
      enable = true;
      systemCronJobs = [
        "* * * * * paho . /etc/profile; /home/paho/dotfiles/hosts/box/ddns.sh &> /tmp/cron.log"
      ];
    };

    # Michael Perlin SSH access
    users.users.perlinm = {
      description = "Michael Perlin";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCnb7DOJt0Npmb7F5HHashy6D0p7A0sMcJBs4JtZZnlhWmcbu53L2rRCuEA4rQdYXCSRR6bOJe72lGFHhb/4Zdkea/+TSVy0AVG4/LTLAUwoBVsvE9PkfpuT4S9W14w+ELM993z8jF66qy5tNF1J6M3nBGIZmfM5Ff053E4/rgdMtrWiVJXx3eRJGFyxUh0dpYH2Nua9MMip7uSt54q5dfOt/IjFSdDUFXqOwypKyTNEMvehiV2wh2IXUTcoFgdlCdaicztrHP3vjY/u6wnmBvf+Mqi7cpxzR0cVXP4Y5xWVupzXFoObTy0vc+LMd2CGyAFXZ4HFfs7u5ZD79NJ9CZkoUduuf9/Rdeugp4nc+rc5CQR9D6n3iWjFAWQInmFJRUHcULYLXaHfKLL2UyTRNlLxsY5pd/VSwd310L63fWBVmtwx5bddWgW+pR7HA+Il1C5q2l5mLvVZvI3hOpEUgCqroABOnAP3uqD0SXjHHRFn7NI6WyZBJ0Q3RaVYE2zmacVujwQSBTdwjF3qjLlfjxmXtx8neo7xWiyU9/oqwnACOiMPzsxTsW8XwL5yHbvHSyAMBO/qzd0WtOv8BNyBcgp8WAf0CZ1KRbMUzMxasYm5q0uWnS04VxyyO9yd9AQHjsU9DAHzdzqUzoGYWdwQY33rukbDwt1pDKaqgBd/2UL4Q== mika.perlin@gmail.com"
      ];
    };
  };
}
