{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.custom = {
    swaylock.color = lib.mkOption {
      type = lib.types.str;
      default = "888888";
    };
  };

  config = {
    stylix = {
      enable = true;
      polarity = "dark";
      base16Scheme = "${pkgs.base16-schemes}/share/themes/seti.yaml";
      fonts = {
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
        gtk.enable = false; # Getting breadcrumbs with light background
        firefox.profileNames = [ "default" ];
      };
    };

    # Program overrides
    programs = {
      alacritty.settings.colors.primary.background = lib.mkForce "0x000000";
      swaylock.settings.color = lib.mkForce config.custom.swaylock.color;
    };

  };
}
