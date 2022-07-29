{ pkgs, ... }:

{
  home.packages = with pkgs; [
    angle-grinder
    arandr
    aws-rotate-key
    bat
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.prettybat
    bind
    # binutils
    bitwarden-cli
    bottom
    broot
    # binutils
    cargo-audit
    cargo-edit
    cargo-expand
    cargo-insta
    cargo-outdated
    cargo-update
    cargo-watch
    choose
    # clang
    difftastic
    diskus
    du-dust
    exa
    fd
    feh
    fzf
    git
    gitAndTools.delta
    gitAndTools.hub
    gnuplot
    haskell-language-server
    htop
    hyperfine
    ion
    jq
    mdcat
    mold
    niv
    nixfmt
    nmap
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    openssl
    pciutils # lspci, etc.
    perl
    # pkg-config
    pgcli
    python3
    # python39Packages.pip
    ripgrep
    rnix-lsp
    ruby
    rust-analyzer
    rustup
    sd
    socat
    solargraph
    sshfs-fuse
    # swift
    taplo-cli
    tokei
    unison
    vim
    vscode-extensions.llvm-org.lldb-vscode
    wally-cli
    wget
    yaml-language-server
    zenith
    zip
    zsh
  ];
}
