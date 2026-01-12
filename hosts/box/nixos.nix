{ lib, pkgs, ... }:
{
  imports = [
    ./auth.nix
    ./authit.nix
    ./ddns.nix
    ./foundry-vtt.nix
    ./hardware-configuration.nix
    ./home-assistant.nix
    ./media.nix
    ./nginx.nix
    ./playlister.nix
    ./vpn.nix
    # TODO: Get working
    # ./wireguard.nix
  ];

  options.custom = {
    ports = lib.mkOption { type = lib.types.attrsOf lib.types.int; };
    uids = lib.mkOption { type = lib.types.attrsOf lib.types.int; };
    groups = lib.mkOption { type = lib.types.attrsOf lib.types.int; };
    drives = lib.mkOption { type = lib.types.attrsOf lib.types.str; };
    ips = lib.mkOption { type = lib.types.attrsOf lib.types.str; };
  };

  config = {
    system.stateVersion = "20.03";
    networking.hostName = "box";

    # Enable graphics for transcoding.
    hardware.graphics = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver
        vpl-gpu-rt
        intel-compute-runtime
      ];
    };

    # Useful for debugging VA-API
    environment.systemPackages = with pkgs; [
      libva-utils
      intel-gpu-tools
    ];

    environment.sessionVariables = {
      LIBVA_DRIVER_NAME = "iHD";
    };
    systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD";

    custom = {
      ports = {
        authit = 43717;
        bitmagnet = 3333;
        bitmagnet_dht = 7405;
        foundry = 8907;
        home_assistant = 8123;
        flaresolverr = 8191;
        jellyfin = 8096;
        kanidm = 8443;
        kanidm_ldap = 3636;
        mqtt = 1833;
        oauth2_proxy = 4180;
        plex = 32400;
        prowlarr = 9696;
        radarr = 7878;
        sonarr = 8989;
        transmission = 9091;
        transmission_peer = 23014;
        rtorrent = 8384;
        rtorrent_scgi = 9092;
        rtorrent_peer = 18279;
        rtorrent_dht = 53228;
        zigbee_frontend = 8099;
      };
      uids = {
        foundry = 982;
      };
      groups = {
        foundry = 981;
        media = 1100;
      };
      drives = {
        storage = "/mnt/storage";
        data = "/data";
      };
      ips = {
        vpn_veth = "10.200.1.2";
      };
    };

    programs.niri.enable = false;

    # Disable USB autosuspend for the Zigbee adapter.
    boot.kernelParams = [ "usbcore.autosuspend=-1" ];

    # ZFS
    # We don't want to use the latest kernel due to ZFS compatibility, which is
    # our default.
    boot.kernelPackages = pkgs.linuxPackages;
    services.zfs.autoScrub.enable = true;
    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.forceImportRoot = false;
    boot.extraModprobeConfig =
      # ZFS tuning initially taken from: https://jrs-s.net/2018/08/17/zfs-tuning-cheat-sheet/
      # Tweaks:
      # * Limit ARC to 16 GiB to leave memory for applications
      let
        zfsOptions = {
          ashift = 12;
          atime = "off";
          compression = "lz4";
          recordsize = "1M";
          xattr = "sa";
          zfs_arc_max = 17179869184;
          # Handle write bursts better
          zfs_dirty_data_max = 6442450944; # 6 GiB
          zfs_txg_timeout = 10; # seconds
        };
        optionsStr = lib.concatStringsSep " " (lib.mapAttrsToList (k: v: "${k}=${toString v}") zfsOptions);
      in
      "options zfs ${optionsStr}";
    networking.hostId = "b0c5b0c5";
    boot.zfs.extraPools = [ "storage" ];

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
