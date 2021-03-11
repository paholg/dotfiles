{ pkgs, ... }:

{
  home.packages = with pkgs; [
    adwaita-qt
    arandr
    (chromium.override {
      commandLineArgs = "--load-media-router-component-extension=1";
    })
    discord
    dmenu
    dzen2
    gimp
    glxinfo
    gnome3.eog
    libnotify
    libreoffice
    okular
    pavucontrol
    scrot
    signal-desktop
    slack
    vlc
    xbrightness
    xdotool
    xorg.xmodmap
    xorg.xrandr
    xournal
  ];
}
