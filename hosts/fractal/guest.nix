{
  lib,
  pkgs,
  ...
}:
let
  gamescopeArgsCommon = [
    "--adaptive-sync" # VRR support
    "--rt"
    "--steam"
  ];
  gamescopeArgsOffice = lib.strings.concatStringsSep " " (
    gamescopeArgsCommon
    ++ [
      # See /sys/class/drm/card* for output names that gamescope uses.
      # They may be different than xrandr.
      "--prefer-output DP-1"
    ]
  );
  gamescopeArgsTv = lib.strings.concatStringsSep " " (
    gamescopeArgsCommon
    ++ [
      "--hdr-enabled"
      # See /sys/class/drm/card* for output names that gamescope uses.
      # They may be different than xrandr.
      "--prefer-output DP-2"
    ]
  );

  steamArgs = lib.strings.concatStringsSep " " [
    "-pipewire-dmabuf"
    "-tenfoot"
  ];
  gamescopeOffice = pkgs.writeShellApplication {
    name = "gamescope-office";
    runtimeInputs = [
      pkgs.gamescope
      pkgs.steam
    ];
    text = "exec gamescope ${gamescopeArgsOffice} -- steam ${steamArgs}";
  };
  gamescopeTv = pkgs.writeShellApplication {
    name = "gamescope-tv";
    runtimeInputs = [
      pkgs.gamescope
      pkgs.steam
    ];
    text = "exec gamescope ${gamescopeArgsTv} -- steam ${steamArgs}";
  };
in
{
  imports = [
    ../../home
  ];
  home.stateVersion = "24.05";

  home.packages = [
    gamescopeOffice
    gamescopeTv
  ];

  custom = {
    username = "guest";
    mangohud.enable = false;
    starship.host_color = "cyan";
  };

  programs.niri.settings = {
    outputs = {
      "DP-1" = {
        enable = false;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 138.0;
        };
      };
      "DP-2" = {
        enable = true;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 60.0;
        };
        scale = 2.0;
      };
    };
    spawn-at-startup = lib.mkForce [
      { command = [ "steam" ]; }
    ];
    workspaces = lib.mkForce { };
    window-rules = lib.mkForce [
      {
        open-maximized = true;
      }
    ];
  };
}
