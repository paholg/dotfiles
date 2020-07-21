{ pkgs, ... }:

{
  imports =
    [
      ../common.nix
    ];

  networking.hostName = "t14s";
  networking.interfaces.enp2s0f0.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  # New kernal required for Ryzen 4750 video card:
  boot.kernelPackages = pkgs.linuxPackages_latest;
  services.xserver.videoDrivers = ["amdgpu"];
}
