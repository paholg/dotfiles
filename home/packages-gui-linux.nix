{pkgs, ...}: {
  home.packages = with pkgs; [
    adwaita-qt
    arandr
    audacity
    blender
    brightnessctl
    chromium
    unfree.discord
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
