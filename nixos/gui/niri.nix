{ pkgs, ... }:
{
  config = {
    programs.niri.enable = true;
    programs.niri.package = pkgs.niri;

    # Workaround for swaylock not accepting password.
    security.pam.services.swaylock = { };

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
