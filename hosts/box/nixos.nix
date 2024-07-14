{ lib, pkgs, ... }:
let
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
  media_gid = 1100;
  storage = /mnt/storage;
in
{
  imports = [
    ./media.nix
    ./vpn.nix
    # ./wireguard.nix
  ];

  system.stateVersion = "20.03";
  networking.hostName = "box";

  custom = {
    ssh.enable = true;

    media = {
      enable = true;
      inherit ports storage;
      gid = media_gid;
      container_ip = ips.container;
    };

    vpn = {
      enable = true;
      inherit ips media_gid storage;
      transmission_port = ports.transmission;
    };
  };

  boot = {
    initrd.availableKernelModules = [
      "xhci_pci"
      "ehci_pci"
      "ahci"
      "mpt3sas"
      "usbhid"
      "usb_storage"
      "sd_mod"
    ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };
  hardware.cpu.intel.updateMicrocode = true;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/ed5f4093-2390-4539-adf0-8fdd721662c1";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/0409-5C67";
      fsType = "vfat";
    };

    "${storage}" = {
      device = "/dev/disk/by-uuid/ce5baea0-d13f-48be-88a1-a8b30b493b5e";
      fsType = "btrfs";
    };
  };
  services.btrfs.autoScrub.enable = true;

  swapDevices = [ ];

  nix.settings.max-jobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

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
}
