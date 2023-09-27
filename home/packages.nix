{pkgs, ...}: {
  home.packages = with pkgs; [
    alejandra # nix formatter
    angle-grinder
    arandr
    aws-rotate-key
    bacon
    bat
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.prettybat
    bind
    # binutils
    bitwarden-cli
    bottom
    broot
    bundix # generate nix expressions for bundler
    cachix
    cargo-audit
    cargo-deny
    cargo-edit
    cargo-expand
    cargo-generate
    cargo-machete
    cargo-outdated
    cargo-update
    cargo-watch
    choose
    curl
    difftastic
    diskus
    du-dust
    eza
    fd
    feh
    fzf
    gh # GitHub CLI
    git
    gitAndTools.delta
    gitAndTools.hub
    gnuplot
    haskell-language-server
    htop
    httpie
    hyperfine
    jq
    jwt-cli
    lld
    marksman # markdown lsp
    mdcat
    neofetch
    nil # nix language server
    niv
    nmap
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    openssl
    pciutils # lspci, etc.
    perl
    # pkg-config
    pgcli
    # python-with-my-packages
    # python310Packages.python-lsp-server
    ripgrep
    rnr # regex rename
    ruby
    rufo
    # rust-analyzer
    rustup
    sd
    shellcheck
    snippets-ls
    socat
    solargraph
    sourcekit-lsp # swift language server
    sshfs-fuse
    swift
    taplo-cli # toml language server
    terraform-ls
    tokei
    unison
    vim
    vscode-langservers-extracted # css, html, json, markdown, eslint
    wally-cli
    wasm-bindgen-cli
    wget
    xsv
    yaml-language-server
    yq-go
    zenith
    zip
    zsh
  ];
}
