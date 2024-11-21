{ config, pkgs, ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
  ];
  home.stateVersion = "20.09";

  custom = {
    username = "paho";
    gui = true;
    linux = true;
    mangohud.enable = true;
    nixos = true;
    starship.host_color = "cyan";
    i3.enable = true;
    i3.customConfig = ''
      exec_always discord
      exec_always steam
      exec_always firefox

      # Float gam
      for_window [class="client"] floating enable
    '';
    fish_extra_init = # fish
      ''
        set TTY (tty)
        [ "$TTY" = "/dev/tty2" ] && exec "startx"
      '';
  };

  home.packages = with pkgs; [
    blender-hip
    discover-overlay
    quickemu # For Windows VM
  ];

  # Store dotfiles in a shared location, so guest can access too:
  home.file.dotfiles.source = config.lib.file.mkOutOfStoreSymlink "/srv/dotfiles";
}
