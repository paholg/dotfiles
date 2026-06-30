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
        net-tools
        outils # sha256, etc.
        psmisc # killall, fuser, etc.
        smartmontools
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
        # kdePackages.kdenlive # video editing, broken 2026-01-29
        kitty.terminfo
        krita
        libreoffice
        yubikey-manager
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
        grim
        guvcview
        kdePackages.dolphin
        kdePackages.okular
        libnotify
        lxqt.lxqt-policykit
        mesa-demos
        pavucontrol
        pulseaudioFull
        satty
        signal-desktop
        slack
        slurp
        vlc
        wl-screenrec
      ]
    else
      [ ];

  defaultPackages = with pkgs; [
    external.agenix
    ast-grep
    aws-rotate-key
    bash-language-server
    bottom
    btop-rocm
    cachix
    choose
    external.claude-code
    external.devconcurrent
    curl
    difftastic
    dig
    dust
    dysk
    entr # Watch for file changes
    erdtree # pretty tree
    eza
    fd
    fzf
    gh # GitHub CLI
    git
    fastfetch
    htop
    hyperfine
    jnv
    jq
    just
    just-lsp
    lazygit
    litecli # sqlite cli
    mkcert
    ncdu # interactive disk-usage
    nh # nix helper
    nil
    nix-output-monitor
    nixd
    onefetch
    openssl
    pciutils # lspci, etc.
    pgcli
    procs # ps
    pv
    python3
    # python3Packages.python-lsp-server
    # python3Packages.python-lsp-ruff
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
    tabiew
    tinymist # typst language server
    tlrc # Shorter man-pages
    tombi # toml language server
    typescript-language-server
    typst
    typstyle # typst formatter
    tokei
    unzip
    vscode-langservers-extracted # css, html, json, markdown, eslint
    wget
    xan
    xh
    yaml-language-server
    yazi
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
