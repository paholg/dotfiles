{ config, pkgs, ... }:
let
  gamescopeSession = pkgs.writeShellApplication {
    name = "gamescope-session";
    runtimeInputs = with pkgs; [
      steam
      gamescope
    ];
    text = # bash
      ''
        gamescopeArgs=(
            --adaptive-sync # VRR support
            # --hdr-enabled
            --rt
            --steam
            # See /sys/class/drm/card* for output names that gamescope uses.
            # They may be different than xrandr.
            --prefer-output DP-1
            --force-grab-cursor
            -s 1.0
        )
        steamArgs=(
            -pipewire-dmabuf
            -tenfoot
        )

        exec gamescope "''${gamescopeArgs[@]}" -- steam "''${steamArgs[@]}"
      '';
  };
in
{
  imports = [
    ../../home
    ./display_switch.nix
  ];
  home.stateVersion = "24.05";

  custom = {
    username = "paho";
    mangohud.enable = true;
    starship.host_color = "cyan";
    swaylock.color = "220044";
    fish_extra_init = # fish
      ''
        set TTY (tty)
        [ "$TTY" = "/dev/tty2" ] && exec "niri-session"
      '';
  };

  programs.niri.settings = {
    outputs = {
      "DP-1" = {
        enable = true;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 138.0;
        };
        variable-refresh-rate = "on-demand";
      };
      "HDMI-A-1" = {
        enable = false;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 120.0;
        };
        scale = 2.0;
        variable-refresh-rate = "on-demand";
      };
    };
    spawn-at-startup = [
      { command = [ "firefox" ]; }
      { command = [ "discord" ]; }
      { command = [ "steam" ]; }
    ];
    workspaces = {
      "01-main".open-on-output = "DP-1";
      "02-chat".open-on-output = "DP-1";
    };
    window-rules = [
      {
        # Float gam
        matches = [ { app-id = "client"; } ];
        open-floating = true;
      }
    ];
  };

  home.packages =
    with pkgs;
    [
      # TODO: broken
      # blender-hip
      discover-overlay
    ]
    ++ [ gamescopeSession ];

  # Store dotfiles in a shared location, so guest can access too:
  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
