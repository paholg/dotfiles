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
    binutils
    cargo-edit
    cargo-kcov
    cargo-outdated
    cargo-update
    cargo-watch
    clang
    clang-tools
    diskus
    exa
    fd
    feh
    fzf
    git
    gitAndTools.delta
    gitAndTools.hub
    gnuplot
    htop
    # jc
    jq
    kcov
    lshw
    niv
    nixfmt
    nmap
    nodePackages.bash-language-server
    openssl
    pciutils # lspci, etc.
    perl
    pkg-config
    psmisc # killall, fuser, etc.
    python-language-server
    python3
    ripgrep
    rnix-lsp
    ruby
    rust-analyzer
    rustup
    socat
    solargraph
    sparse
    sshfs-fuse
    strace
    unison
    usbutils
    wget
    zsh
  ];
}
