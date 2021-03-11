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
    nmap
    nodePackages.bash-language-server
    openssl
    pciutils # lspci, etc.
    perl
    pkg-config
    psmisc # killall, fuser, etc.
    python-language-server
    ripgrep
    rnix-lsp
    ruby
    rust-analyzer
    rustup
    socat
    sparse
    sshfs-fuse
    strace
    unison
    usbutils
    wget
    zsh
  ];
}
