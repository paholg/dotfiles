{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = {
    stylix = {
      enable = true;
      enableReleaseChecks = false;
      # Incompatible with useGlobalPkgs:
      overlays.enable = false;
      polarity = "dark";
      base16Scheme = "${pkgs.base16-schemes}/share/themes/seti.yaml";
      fonts = {
        sizes.terminal = 12;
        monospace = {
          name = "Monaspace Neon";
          package = pkgs.monaspace;
        };
      };
      cursor = {
        package = pkgs.phinger-cursors;
        name = "phinger-cursors-light";
        size = 32;
      };
      opacity = {
        popups = 0.9;
        terminal = 0.95;
      };
      targets = {
        helix.enable = false;
        # Stylix does not do a good job with GTK.
        gtk.enable = false;
        kitty.variant256Colors = true;
        firefox.profileNames = [ "default" ];
        waybar.enable = false;
      };
    };

    gtk = {
      enable = true;
      theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome-themes-extra;
      };
    };

    # Program overrides
    programs = {
      alacritty.settings.colors.primary.background = lib.mkForce "0x000000";
      kitty.extraConfig = ''
        font_family family='Monaspace Neon' style=Light features="+calt +liga +ss01 +ss03 +ss04 +ss02 +ss05 +ss07 +ss09 +ss10"
      '';
      swaylock.settings.color = lib.mkForce config.custom.swaylock.color;
    };
  };
}
