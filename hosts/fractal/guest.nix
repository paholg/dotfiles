{
  config,
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
      "--prefer-output HDMI-A-1"
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

    fish_extra_init = # fish
      ''
        set TTY (tty)
        # TTY1: startx
        [ "$TTY" = "/dev/tty1" ] && ${lib.getExe gamescopeTv} 2>&1 | tee gamescope.log
      '';
  };

  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
