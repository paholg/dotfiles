{ pkgs, ... }:

{
  home.packages = with pkgs; [
    adwaita-qt
    arandr
    (chromium.override {
      commandLineArgs = "--load-media-router-component-extension=1";
    })
    dmenu
    dzen2
    gimp
    gnome3.eog
    libreoffice
    okular
    pavucontrol
    scrot
    signal-desktop
    vlc
    xbrightness
    xorg.xmodmap
    xorg.xrandr
    xournal
  ];
}
