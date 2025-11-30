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
        inlyne # markdown viewer
        kdePackages.kdenlive # video editing
        krita
        libreoffice
        mdbook
        pinta # paint
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
        guvcview
        kdePackages.dolphin
        kdePackages.okular
        libnotify
        lxqt.lxqt-policykit
        mesa-demos
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
    claude-code
    curl
    difftastic
    dig
    dust
    dysk
    entr # Watch for file changes
    external.envswitch
    erdtree # pretty tree
    eza
    fd
    feh
    fzf
    gh # GitHub CLI
    git
    github-cli
    fastfetch
    htop
    hyperfine
    jless
    jq
    just
    just-lsp
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
    pv
    python3
    python3Packages.python-lsp-server
    python3Packages.python-lsp-ruff
    python3Packages.ruff
    ripgrep
    rnr # regex rename
    ruby_3_4
    rustup
    sd
    serpl # search and replace
    shellcheck
    socat
    sshfs-fuse
    superhtml # html lsp
    tabiew
    taplo # toml language server
    tinymist # typst language server
    tlrc # Shorter man-pages
    typst
    tokei
    unzip
    vscode-langservers-extracted # css, html, json, markdown, eslint
    wget
    xan
    xh
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
