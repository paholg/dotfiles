{ pkgs, ... }:
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
      # Pinned: 0.8.2 dismisses Steam's menus the instant they open
      # (https://github.com/Supreeeme/xwayland-satellite/issues/503).
      # Drop once a fixed release lands in nixpkgs.
      (pkgs.xwayland-satellite.overrideAttrs (
        _: rec {
          version = "0.8";
          src = pkgs.fetchFromGitHub {
            owner = "Supreeeme";
            repo = "xwayland-satellite";
            tag = "v${version}";
            hash = "sha256-Qz1WvGdawnoz4dG3JtCtlParmdQHM5xu6osnXeVOqYI=";
          };
          cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
            inherit src;
            hash = "sha256-HGrMjNIsUqh8AFtSABk615x4B9ygrVEn26V0G1kX/nA=";
          };
          # 0.8 has no man page.
          outputs = [ "out" ];
          postInstall = ''
            install -Dm0644 resources/xwayland-satellite.service -t $out/lib/systemd/user
          '';
        }
      ))
    ];

    programs = {
      rofi.enable = true;
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
    systemd.user.packages = [ pkgs.mako ];
  };
}
