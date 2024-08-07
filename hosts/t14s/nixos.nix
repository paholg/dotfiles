{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  system.stateVersion = "24.05";
  networking.hostName = "t14s";

  custom = {
    gui = true;
    ssh = true;
  };

  boot.initrd.luks.devices."luks-8bf7e340-215e-41cb-9f32-3833204fab54".device = "/dev/disk/by-uuid/8bf7e340-215e-41cb-9f32-3833204fab54";

  # For display-switch
  hardware.i2c.enable = true;

  # DisplayLink proprietary drivers for USB hubs.
  # NOTE: Needs to be manually downloaded :(
  services.xserver.videoDrivers = [
    "displaylink"
    "modesetting"
  ];

  networking.networkmanager.enable = true;

  hardware.graphics = {
    enable = true;

    enable32Bit = true;
    # amdvlk: open-source Vulkan driver from AMD
    extraPackages = [ pkgs.amdvlk ];
    extraPackages32 = [ pkgs.driversi686Linux.amdvlk ];
  };

}
