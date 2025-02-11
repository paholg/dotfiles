{ config, pkgs, ... }:
let
  cfg = config.custom;

  linux =
    if cfg.linux then
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

  gui =
    if cfg.gui then
      with pkgs;
      [
        dialog
        dmenu
        libreoffice
        mdbook
        inlyne # markdown viewer
        nerd-fonts.fira-code
      ]
    else
      [ ];

  wayland = if cfg.wayland then [ ] else [ ];

  x11 =
    if cfg.x11 then
      with pkgs;
      [
        arandr
        external.rustybar
        xclip
        xdotool
        xorg.xev
        xorg.xkill
        xsel
        xterm
      ]
    else
      [ ];

  linux-gui =
    if cfg.linux && cfg.gui then
      with pkgs;
      [
        adwaita-qt
        audacity
        brightnessctl
        chromium
        dconf
        ddcutil
        dolphin
        dzen2
        eog
        evince
        gimp
        glxinfo
        krita
        libnotify
        lxappearance
        lxqt.lxqt-policykit
        okular
        pavucontrol
        pulseaudioFull
        scrot
        signal-desktop
        discord
        slack
        vlc
        zoom-us
      ]
    else
      [ ];

  default = with pkgs; [
    # iredis
    external.agenix
    angle-grinder
    arandr
    ast-grep
    aws-rotate-key
    bacon
    bash-language-server
    # TODO: broken
    # bitwarden-cli
    bottom
    broot
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
    jq
    just
    lazygit
    litecli # sqlite cli
    markdown-oxide
    marksman # markdown lsp
    ncdu # interactive disk-usage
    nh # nix helper
    nix-output-monitor
    nodePackages.typescript-language-server
    onefetch
    openssl
    pciutils # lspci, etc.
    pgcli
    ripgrep
    rnr # regex rename
    ruby
    rustup
    sd
    serpl # search and replace
    shellcheck
    socat
    sshfs-fuse
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
    zip
  ];
in
{
  config = {
    home.packages = default ++ linux ++ gui ++ linux-gui ++ wayland ++ x11;
  };
}
