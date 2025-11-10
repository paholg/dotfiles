{ lib, pkgs, ... }:
{
  config = {
    programs.niri.enable = true;

    # Workaround for swaylock not accepting password.
    security.pam.services.swaylock = { };

    services.displayManager.gdm = {
      enable = true;
    };

    # Configure GDM login screen to turn off monitors after 5 seconds
    programs.dconf.profiles.gdm.databases = [{
      settings = {
        "org/gnome/desktop/session" = {
          idle-delay = lib.gvariant.mkUint32 5;
        };
      };
    }];

    # Desktop portal stuff -- all this stuff might not be needed.
    xdg.portal = {
      enable = true;
      wlr.enable = false;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gnome
      ];
      config = {
        common.default = [ "gnome" ];
        screencast.default = [ "gnome" ];
      };
    };
  };
}
