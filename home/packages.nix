{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpi
    arandr
    bat
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.prettybat
    bind
    direnv
    exa
    fd
    feh
    fzf
    git
    gitAndTools.delta
    htop
    jq
    lshw
    nixfmt
    openssl
    pciutils # lspci, etc.
    perl
    pkg-config
    psmisc # killall, fuser, etc.
    ripgrep
    ruby
    rust-analyzer
    rustup
    sshfs-fuse
    strace
    usbutils
    wget
    zsh
  ];
}
