{ pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
    ];

  system.stateVersion = "20.03";
  system.autoUpgrade.enable = true;

  boot = {
    kernelParams = [
      "consoleblank=60"
    ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking.networkmanager.enable = true;
  networking.useDHCP = false;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    earlySetup = true;
    font = "ter-i32b";
    packages = [pkgs.terminus_font];
    keyMap = "us";
  };
  time.timeZone = "America/Los_Angeles";
  nixpkgs.config.allowUnfree = true;

  users.users.paho = {
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" ];
  };

  services = {
    emacs.enable = true;
  };

  environment.systemPackages = with pkgs; [
    acpi
    arandr
    diffr
    emacs
    exa
    feh
    git
    htop
    keychain
    lshw
    nix-index
    openssl
    pciutils # lspci, etc.
    pkg-config
    psmisc # killall, fuser, etc.
    ripgrep
    rust-analyzer
    rustup
    wget
    zsh
  ];
}
