{ pkgs, ... }:

{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
    ];

  system.stateVersion = "20.03";
  system.autoUpgrade.enable = true;

  boot = {
    earlyVconsoleSetup = true;
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
    # font = "ter-i32b";
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
    pciutils # lspci, etc.
    psmisc # killall, fuser, etc.
    ripgrep
    rust-analyzer
    rustup
    wget
    zsh
  ];
}
