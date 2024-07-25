{ config, pkgs, ... }:
let
  cfg = config.custom;

  linux =
    if cfg.linux then
      with pkgs;
      [
        acpi
        cargo-kcov
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
        # TODO: Broken
        # inlyne # markdown viewer
        (nerdfonts.override { fonts = [ "FiraCode" ]; })
      ]
    else
      [ ];

  wayland = if cfg.wayland then [ ] else [ ];

  x11 =
    if cfg.x11 then
      with pkgs;
      [
        xclip
        xdotool
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
        arandr
        audacity
        blender
        brightnessctl
        chromium
        dconf
        dolphin
        dzen2
        gimp
        glxinfo
        gnome.eog
        krita-beta
        libnotify
        lxappearance
        lxqt.lxqt-policykit
        okular
        pavucontrol
        rustybar
        scrot
        signal-desktop
        unfree.discord
        unfree.slack
        vlc
        zoom-us
      ]
    else
      [ ];

  default = with pkgs; [
    # iredis
    agenix
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
    dysk
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
    jq
    just
    jwt-cli
    litecli
    lld
    lsd
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
    nixfmt-rfc-style
    nls # nickel language server
    nmap
    nodePackages.typescript-language-server
    onefetch
    openssl
    pciutils # lspci, etc.
    perl
    pgcli
    pgformatter
    ra-multiplex
    rip
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
    unzip
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
  ];
in
{
  config = {
    home.packages = default ++ linux ++ gui ++ linux-gui ++ wayland ++ x11;
  };
}
