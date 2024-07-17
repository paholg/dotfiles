{ lib, pkgs, ... }:
{
  imports = [
    ./gui.nix
    ./ssh.nix
  ];
  boot = {
    kernelParams = [ "consoleblank=600" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  # Enable all firmware regardless of license.
  hardware.enableAllFirmware = true;

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  nix = {
    package = pkgs.nix;
    settings.auto-optimise-store = true;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
    settings.max-jobs = "auto";
    settings.trusted-users = [ "paho" ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    # spacemacs colors:
    colors = [
      "1f2022"
      "f2241f"
      "67b11d"
      "b1951d"
      "4f97d7"
      "a31db1"
      "2d9574"
      "a3a3a3"
      "585858"
      "f2241f"
      "67b11d"
      "b1951d"
      "4f97d7"
      "a31db1"
      "2d9574"
      "f8f8f8"
    ];
    earlySetup = true;
    font = "ter-i32b";
    packages = [ pkgs.terminus_font ];
    keyMap = "us";
  };
  time.timeZone = "America/Los_Angeles";

  users.users.paho = {
    shell = pkgs.bash;
    isNormalUser = true;
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };

  services.fwupd.enable = true;
  services.resolved = {
    enable = true;
    extraConfig = ''
      [Resolve]
      DNS=45.90.28.0#827fdd.dns.nextdns.io
      DNS=2a07:a8c0::#827fdd.dns.nextdns.io
      DNS=45.90.30.0#827fdd.dns.nextdns.io
      DNS=2a07:a8c1::#827fdd.dns.nextdns.io
      DNSOverTLS=yes
    '';
  };
}
