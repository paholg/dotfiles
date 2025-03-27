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
    gui = true;
    linux = true;
    nixos = true;
    # mangohud.enable = true;
    starship.host_color = "cyan";
    niri.enable = true;

    # i3.customConfig = ''
    #   exec_always discord
    #   exec_always steam
    #   exec_always firefox

    #   # Float gam
    #   for_window [class="client"] floating enable
    # '';
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
        position.x = 0;
        position.y = 0;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 138.0;
        };
        # variable-refresh-rate = "on-demand";
      };
      "HDMI-A-1" = {
        enable = false;
        # position.x = 0;
        # position.y = 0;
        mode = {
          width = 3840;
          height = 2160;
          refresh = 120.0;
        };
        scale = 2.0;
        # variable-refresh-rate = "on-demand";
      };
    };
  };

  home.packages =
    with pkgs;
    [
      # TODO: broken
      # blender-hip
      discover-overlay
      quickemu # For Windows VM
    ]
    ++ [ gamescopeSession ];

  # Store dotfiles in a shared location, so guest can access too:
  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
