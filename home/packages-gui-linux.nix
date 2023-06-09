{ pkgs, ... }:

{
  home.packages = with pkgs; [
    adwaita-qt
    arandr
    audacity
    blender
    brightnessctl
    chromium
    discord
    dzen2
    gimp
    glxinfo
    gnome.eog
    libnotify
    # libreoffice
    lxappearance
    lxqt.lxqt-policykit
    # mangohud
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
  ];
}
