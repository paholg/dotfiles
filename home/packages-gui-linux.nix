{ pkgs, ... }:

{
  home.packages = with pkgs; [
    adwaita-qt
    arandr
    # chromium
    brightnessctl
    discord
    dzen2
    gimp
    glxinfo
    gnome3.eog
    libnotify
    libreoffice
    lxqt.lxqt-policykit
    okular
    pavucontrol
    scrot
    signal-desktop
    slack
    vlc
    xclip
    xdotool
    xorg.xmodmap
    xorg.xrandr
    xournal
    xsel
    zoom-us
  ];
}
