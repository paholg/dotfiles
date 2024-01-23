{...}: {
  imports = [
    ../../nix/common.nix
    ../../nix/ssh.nix
    ./media.nix
    ./vpn.nix
    # ./wireguard.nix
  ];

  swapDevices = [
    {
      device = "/swapfile";
      priority = 100;
      size = 16384;
    }
  ];

  fileSystems = {
    "/mnt/storage" = {
      device = "/dev/disk/by-uuid/ce5baea0-d13f-48be-88a1-a8b30b493b5e";
      fsType = "btrfs";
    };
  };
  services.btrfs.autoScrub.enable = true;

  networking.hostName = "box";

  services.cron = {
    enable = true;
    systemCronJobs = [
      "* * * * * paho . /etc/profile; /home/paho/dotfiles/hosts/box/ddns.sh &> /tmp/cron.log"
    ];
  };
}
