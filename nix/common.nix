{pkgs, ...}: {
  imports = [/etc/nixos/hardware-configuration.nix];

  system.stateVersion = "20.03";
  boot = {
    kernelParams = ["consoleblank=30"];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  nix = {
    package = pkgs.nix;
    settings.auto-optimise-store = true;
    settings.experimental-features = ["nix-command" "flakes"];
    settings.max-jobs = "auto";
    settings.trusted-users = ["paho"];
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  environment = {pathsToLink = ["/share/zsh"];};

  # networking.networkmanager.enable = true;
  # networking.useDHCP = false;

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
    packages = [pkgs.terminus_font];
    keyMap = "us";
  };
  time.timeZone = "America/Los_Angeles";

  users.users.paho = {
    shell = pkgs.bash;
    isNormalUser = true;
    extraGroups = ["networkmanager" "wheel"];
  };

  programs.zsh.enable = true;
}
