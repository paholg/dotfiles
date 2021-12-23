{ pkgs, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      zoom-us = super.zoom-us.overrideAttrs (old: rec {
        version = "5.7.29123.0808";

        src = self.fetchurl {
          url = "https://zoom.us/client/${version}/zoom_x86_64.pkg.tar.xz";
          sha256 = "WAeE/2hUaQbWwDg/iqlKSZVoH3ruVHvh+9SEWdPwCIc=";
        };
      });
    })
  ];

  home.packages = with pkgs; [
    adwaita-qt
    arandr
    (chromium.override {
      commandLineArgs = "--load-media-router-component-extension=1";
    })
    brightnessctl
    dialog
    discord
    dmenu
    dzen2
    gimp
    glxinfo
    gnome3.eog
    googleearth
    libnotify
    libreoffice
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    okular
    pavucontrol
    scrot
    signal-desktop
    slack
    vlc
    xdotool
    xorg.xmodmap
    xorg.xrandr
    xournal
    zoom-us
  ];
}
