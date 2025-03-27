{ pkgs, ... }:
{
  config = {
    programs.niri.enable = true;

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
    environment.systemPackages = with pkgs; [
      xdg-desktop-portal
      xdg-desktop-portal-gnome
    ];
    # Potential workaround for zoom nonsense
    # See https://github.com/NixOS/nixpkgs/issues/359533#issuecomment-2743885369
    system.activationScripts.xdg-portals.text = ''
      #!/usr/bin/env bash
      set -eu

      echo "Setting up xdg-desktop-portal-gnome and xdg-desktop-portal for zoom-us"

      mkdir -p /usr/share/xdg-desktop-portal
      rm -f /usr/share/xdg-desktop-portal/portals
      ln -s  /run/current-system/sw/share/xdg-desktop-portal/portals /usr/share/xdg-desktop-portal/portals

      mkdir -p /usr/libexec
      rm -f /usr/libexec/xdg-desktop-portal-gnome /usr/libexec/xdg-desktop-portal
      ln -s  ${pkgs.xdg-desktop-portal-gnome}/libexec/xdg-desktop-portal-gnome /usr/libexec/xdg-desktop-portal-gnome
      ln -s  ${pkgs.xdg-desktop-portal}/libexec/xdg-desktop-portal /usr/libexec/xdg-desktop-portal

      mkdir -p /usr/lib64
      rm -f /usr/lib64/xdg-desktop-portal-gnome /usr/lib64/xdg-desktop-portal
      ln -s  ${pkgs.xdg-desktop-portal-gnome}/lib64/xdg-desktop-portal-gnome /usr/lib64/xdg-desktop-portal-gnome
      ln -s  ${pkgs.xdg-desktop-portal}/lib64/xdg-desktop-portal /usr/lib64/xdg-desktop-portal
    '';
  };
}
