{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  system.stateVersion = "23.11";
  networking.hostName = "frame";

  custom = {
    gui = true;
    ssh = true;
  };

  boot.initrd.luks.devices."luks-0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb".device = "/dev/disk/by-uuid/0f2fe45b-6e0e-4cb6-b9ee-87b639fb04cb";

  networking.networkmanager.enable = true;

  hardware.graphics = {
    enable = true;

    enable32Bit = true;
    # amdvlk: open-source Vulkan driver from AMD
    extraPackages = [ pkgs.amdvlk ];
    extraPackages32 = [ pkgs.driversi686Linux.amdvlk ];
  };

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    defaultNetwork.settings.dns_enabled = true;
  };
}
