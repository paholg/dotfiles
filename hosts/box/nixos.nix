{ lib, pkgs, ... }:
{
  imports = [
    ./auth.nix
    ./authit.nix
    ./cookit.nix
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
      smartmontools
    ];

    environment.sessionVariables = {
      LIBVA_DRIVER_NAME = "iHD";
    };
    systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD";

    custom = {
      ports = {
        authit = 43717;
        cookit = 30071;
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
        data = "/mnt/data";
        storage = "/mnt/storage";
      };
      ips = {
        vpn_veth = "10.200.1.2";
      };
    };

    programs.niri.enable = false;

    # Disable USB autosuspend for the Zigbee adapter.
    boot.kernelParams = [ "usbcore.autosuspend=-1" ];

    # Disable SATA link power management (ALPM/DIPM) to prevent drives from
    # initiating low-power link transitions that cause COMWAKE link resets.
    # The Exos X16 (ST16000NM001G) has 140 command timeouts from this.
    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="scsi_host", KERNEL=="host*", \
        ATTR{link_power_management_policy}="max_performance"
    '';

    # Recovery from kernel/service hangs.
    #
    # In May 2026 a ZFS LZ4 oops left tasks stuck in D state; the box couldn't
    # shut down and needed a physical power cycle. These settings make the
    # kernel reboot itself rather than limp on with corrupted state:
    #
    # - panic_on_oops: turn a kernel oops (like the LZ4 one) into a panic so it
    #   actually reboots instead of leaving the kernel tainted and ZFS wedged.
    # - panic=10: reboot 10s after panic so logs flush.
    # - hung_task_panic + 300s timeout: panic if any task is stuck in D state
    #   for 5 minutes. 300s (vs the 120s default warning) leaves headroom for
    #   slow rtorrent/ZFS bursts.
    # - RuntimeWatchdogSec arms the hardware watchdog (intel_oc_wdt) so a
    #   fully hung PID 1 still reboots.
    # - DefaultTimeoutStopSec shortens shutdown when services misbehave so
    #   we don't sit on 90s × N service stop timeouts.
    boot.kernel.sysctl = {
      "kernel.panic_on_oops" = 1;
      "kernel.panic" = 10;
      "kernel.hung_task_panic" = 1;
      "kernel.hung_task_timeout_secs" = 300;
    };
    systemd.settings.Manager = {
      RuntimeWatchdogSec = "60";
      RebootWatchdogSec = "120";
      DefaultTimeoutStopSec = "30s";
    };

    # Swap to prevent OOM kills under heavy transcoding load.
    swapDevices = [
      {
        device = "/var/swapfile";
        size = 32768;
      }
    ];

    # ZFS + drive health monitoring
    # Pin to the 6.12 LTS kernel. The latest stable (6.18 in May 2026) hit a
    # ZFS LZ4 decompression oops on this host; LTS + ZFS is a better-tested
    # combination for a 24/7 storage server.
    boot.kernelPackages = pkgs.linuxPackages_6_12;
    services.zfs.autoScrub.enable = true;

    # Monitor all drives: track all SMART attributes (-a), enable automatic
    # offline data collection (-o on), and schedule self-tests:
    #   S/../.././02  = short test daily at 02:00
    #   L/../../1/03  = long test on Mondays at 03:00
    # Alerts appear in `journalctl -u smartd`.
    services.smartd = {
      enable = true;
      defaults.monitored = "-a -o on -s (S/../.././02|L/../../1/03)";
    };
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
    boot.zfs.extraPools = [
      "data"
      "storage"
    ];

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
