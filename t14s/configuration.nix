{ config, lib, pkgs, ... }:

{
  imports = [ ../nix/common.nix ../nix/gui.nix ];

  swapDevices = [{
    device = "/swapfile";
    priority = 100;
    size = 32768;
  }];

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  hardware.enableAllFirmware = true;
  boot = { kernelParams = [ "iommu=pt" ]; };

  networking = {
    hostName = "t14s";
    useNetworkd = true;
    useDHCP = false;
  };

  # New kernal required for Ryzen 4750 video card:
  boot.kernelPackages = pkgs.linuxPackages_latest;
  services.xserver.videoDrivers = [ "amdgpu" ];

  hardware = {
    enableRedistributableFirmware = true;
    opengl = {
      enable = true;
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    };
    pulseaudio.support32Bit = true;
    steam-hardware.enable = true;
  };
  environment.systemPackages = with pkgs; [ steam ];
}
