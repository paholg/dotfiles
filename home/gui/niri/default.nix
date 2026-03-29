{ lib, pkgs, ... }:
let
  nkillPkg = pkgs.writeShellApplication {
    name = "nkill";
    runtimeInputs = with pkgs; [
      niri
      ripgrep
    ];
    text = # bash
      ''
        kill "$@" "$(niri msg focused-window | rg --trim -r '$1' "PID: (\d+)")"
      '';
  };
in
{
  imports = [
    ./background.nix
    ./locker.nix
    ./mark-urgent.nix
    ./rustybar.nix
  ];

  config = {
    home.packages = [
      nkillPkg
      pkgs.imv
      pkgs.nautilus # File-picker used by our desktop portal
      pkgs.wl-clipboard-rs
      pkgs.xwayland-satellite
    ];

    programs = {
      fuzzel.enable = true;
      swaylock = {
        enable = true;
        settings = {
          show-failed-attempts = true;
        };
      };
    };

    services = {
      gammastep = {
        enable = true;
        latitude = 47.6;
        longitude = -122.3;
        tray = true;
        temperature = {
          day = 6500;
          night = 4500;
        };
      };

      mako = {
        enable = true;
        settings.default-timeout = 60000;
      };
    };
  };
}
