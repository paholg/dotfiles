{pkgs, ...}: {
  home.packages = with pkgs; [
    adwaita-qt
    arandr
    audacity
    blender
    brightnessctl
    chromium
    unfree.discord
    dolphin
    dzen2
    gimp
    glxinfo
    gnome.eog
    krita-beta
    libnotify
    # libreoffice
    lxappearance
    lxqt.lxqt-policykit
    # mangohud
    okular
    pavucontrol
    rustybar
    scrot
    signal-desktop
    unfree.slack
    vlc
    xclip
    xdotool
    xorg.xmodmap
    xorg.xrandr
    xournal
    xsel
  ];
}
