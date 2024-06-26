{pkgs, ...}: {
  home.packages = with pkgs; [
    alejandra # nix formatter
    angle-grinder
    arandr
    aws-rotate-key
    bacon
    bash-language-server
    bat
    bat-extras.batgrep
    bat-extras.batman
    bat-extras.prettybat
    bind
    bitwarden-cli
    bottom
    broot
    bundix # generate nix expressions for bundler
    cachix
    cargo-deny
    cargo-duplicates
    cargo-edit
    cargo-expand
    cargo-flamegraph
    cargo-generate
    cargo-machete
    cargo-nextest
    cargo-outdated
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
    helm-ls
    htop
    httpie
    hyperfine
    # iredis
    jq
    just
    jwt-cli
    litecli
    lld
    marksman # markdown lsp
    mdcat
    mosh
    ncdu # interactive disk-usage
    neofetch
    nh # nix helper
    nickel
    nil # nix language server
    niv
    nix-output-monitor
    nixpkgs-fmt
    nls # nickel language server
    nmap
    nodePackages.typescript-language-server
    openssl
    pciutils # lspci, etc.
    perl
    # pkg-config
    pgcli
    pgformatter
    ra-multiplex
    ripgrep
    rnr # regex rename
    ruby
    rufo
    rustup
    sd
    shellcheck
    socat
    solargraph
    sshfs-fuse
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
