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
      "consoleblank=30"
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
    # spacemacs colors:
    colors = [
      "0x1f2022"
      "0xf2241f"
      "0x67b11d"
      "0xb1951d"
      "0x4f97d7"
      "0xa31db1"
      "0x2d9574"
      "0xa3a3a3"
      "0x585858"
      "0xf2241f"
      "0x67b11d"
      "0xb1951d"
      "0x4f97d7"
      "0xa31db1"
      "0x2d9574"
      "0xf8f8f8"
    ];
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
    dash
    diffr
    emacs
    exa
    feh
    git
    htop
    jq
    keychain
    lshw
    nix-index
    openssl
    pciutils # lspci, etc.
    pkg-config
    psmisc # killall, fuser, etc.
    ripgrep
    ruby
    rust-analyzer
    rustup
    sshfs-fuse
    wget
    zsh
  ];
}
