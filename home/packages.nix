{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpi
    arandr
    exa
    feh
    git
    gitAndTools.delta
    htop
    jq
    lshw
    nixfmt
    openssl
    pciutils # lspci, etc.
    pkg-config
    psmisc # killall, fuser, etc.
    ripgrep
    ruby
    rust-analyzer
    rustup
    sshfs-fuse
    wget
    zsh
  ];
}
