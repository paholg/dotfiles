{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  system.stateVersion = "24.05";
  networking.hostName = "t14s";

  custom = {
    gui = true;
    next_dns = true;
    ssh = false;
  };

  # For display-switch
  hardware.i2c.enable = true;

  networking.networkmanager.enable = false;
  networking.wireless.enable = false;
}
