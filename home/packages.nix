{
  gui,
  linux,
  pkgs,
  ...
}:
let
  linuxPackages =
    if linux then
      with pkgs;
      [
        acpi
        dmidecode
        jc
        kcov
        lshw
        outils # sha256, etc.
        psmisc # killall, fuser, etc.
        sparse
        strace
        usbutils
      ]
    else
      [ ];

  guiPackages =
    if gui then
      with pkgs;
      [
        libreoffice
        mdbook
        inlyne # markdown viewer
      ]
    else
      [ ];

  linuxGuiPackages =
    if linux && gui then
      with pkgs;
      [
        audacity
        brightnessctl
        chromium
        dconf
        ddcutil
        discord
        eog
        evince
        # gimp # broken
        glxinfo
        guvcview
        kdePackages.dolphin
        kdePackages.okular
        krita
        libnotify
        lxappearance
        lxqt.lxqt-policykit
        pavucontrol
        pulseaudioFull
        signal-desktop-bin
        slack
        vlc
      ]
    else
      [ ];

  defaultPackages = with pkgs; [
    external.agenix
    angle-grinder
    arandr
    ast-grep
    aws-rotate-key
    bash-language-server
    # TODO: broken
    # bitwarden-cli
    bottom
    broot
    btop-rocm
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
    choose
    curl
    difftastic
    dig
    du-dust
    dysk
    entr # Watch for file changes
    erdtree # pretty tree
    eza
    fd
    feh
    fzf
    gh # GitHub CLI
    git
    gitAndTools.delta
    gitAndTools.hub
    fastfetch
    htop
    httpie
    hyperfine
    jless
    jq
    just
    kitty.terminfo
    lazygit
    litecli # sqlite cli
    markdown-oxide
    marksman # markdown lsp
    ncdu # interactive disk-usage
    nh # nix helper
    nix-output-monitor
    nix-search
    nixd
    nodePackages.typescript-language-server
    onefetch
    openssl
    pciutils # lspci, etc.
    pgcli
    procs # ps
    python3Full
    python3Packages.python-lsp-server
    python3Packages.python-lsp-ruff
    python3Packages.ruff
    ripgrep
    rnr # regex rename
    ruby
    rustup
    sd
    serpl # search and replace
    shellcheck
    socat
    sshfs-fuse
    superhtml # html lsp
    taplo-cli # toml language server
    tinymist # typst language server
    tlrc # Shorter man-pages
    typst
    tokei
    unzip
    vscode-langservers-extracted # css, html, json, markdown, eslint
    wget
    xan
    yaml-language-server
    yq-go
    zellij
    zip
  ];
in
{
  config = {
    home.packages = defaultPackages ++ linuxPackages ++ guiPackages ++ linuxGuiPackages;
  };
}
