{ pkgs, ... }:

{
  home.packages = with pkgs; [
    arandr
    bat
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.prettybat
    bind
    cargo-edit
    cargo-outdated
    cargo-update
    cargo-watch
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
    ion
    jq
    niv
    nixfmt
    nmap
    nodePackages.bash-language-server
    openssl
    pciutils # lspci, etc.
    perl
    pkg-config
    python3
    ripgrep
    rnix-lsp
    ruby
    rust-analyzer
    rustup
    socat
    solargraph
    sshfs-fuse
    unison
    wget
    zsh
  ];
}
