{
  config,
  lib,
  pkgs,
  ...
}:
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
            --hdr-enabled
            --rt
            --steam
            # See /sys/class/drm/card* for output names that gamescope uses.
            # They may be different than xrandr.
            --prefer-output HDMI-A-1
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
    username = "guest";
    gui = true;
    linux = true;
    mangohud.enable = false;
    nixos = true;
    starship.host_color = "cyan";
    # xfce.enable = true;
    i3.enable = true;
    fish_extra_init = # fish
      ''
        set TTY (tty)
        # TTY1: startx
        [ "$TTY" = "/dev/tty1" ] && ${lib.getExe gamescopeSession} 2>&1 | tee gamescope.log
      '';
  };

  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
