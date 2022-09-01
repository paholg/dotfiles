{ config, lib, pkgs, ... }:

{
  imports = [ ../nix/common.nix ../nix/gui.nix ];

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  hardware.enableAllFirmware = true;
  boot = {
    kernelParams = [ "iommu=pt" ];
    initrd.kernelModules = [ "amdgpu" ];
  };

  networking = {
    hostName = "t14s";
    useNetworkd = true;
    useDHCP = false;
  };

  # New kernal required for Ryzen 4750 video card:
  boot.kernelPackages = pkgs.linuxPackages_latest;
  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" ];
  };
  hardware = {
    enableRedistributableFirmware = true;
    opengl = {
      enable = true;
      # Enable Vulkan
      driSupport = true;
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    };
    pulseaudio.support32Bit = true;
    steam-hardware.enable = true;
  };
  environment.systemPackages = with pkgs; [ steam ];

  users.users.paho.extraGroups = [ "docker" ];
  virtualisation.docker.enable = true;
}
