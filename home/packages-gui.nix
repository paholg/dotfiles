{ pkgs, ... }:

{
  home.packages = with pkgs; [
    adwaita-qt
    arandr
    (chromium.override {
      commandLineArgs = "--load-media-router-component-extension=1";
    })
    brightnessctl
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
  ];
}
