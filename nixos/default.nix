{
  gui,
  lib,
  pkgs,
  ...
}:
let
  secret_files = with builtins; filter (f: f != "secrets.nix") (attrNames (readDir ../secrets));
  secrets = builtins.listToAttrs (
    map (f: {
      name = f;
      value = {
        file = ../secrets/${f};
      };
    }) secret_files
  );
in
{
  imports = [
    ./ssh.nix
  ] ++ (if gui then [ ./gui ] else [ ]);

  config = {
    boot = {
      kernelParams = [ "consoleblank=600" ];

      loader = {
        systemd-boot = {
          enable = true;
          memtest86.enable = true;
          configurationLimit = 10;
        };
        efi.canTouchEfiVariables = true;
      };
    };

    boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;

    # Enable all firmware regardless of license.
    hardware.enableAllFirmware = true;
    hardware.enableRedistributableFirmware = true;

    networking.useDHCP = lib.mkDefault true;
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    age.secrets = secrets;

    nix = {
      package = lib.mkDefault pkgs.nix;
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
        persistent = true;
      };
    };

    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };

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

    # Use Quad9 for DNS
    networking.nameservers = [
      "9.9.9.9"
      "149.112.112.112"
    ];
  };
}
