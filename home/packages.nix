{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.custom.packages;

  linux =
    with pkgs;
    if cfg.linux then
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
    with pkgs;
    if cfg.gui then
      [
        dialog
        dmenu
        inlyne # markdown viewer
        (nerdfonts.override { fonts = [ "FiraCode" ]; })
      ]
    else
      [ ];

  linux-gui =
    with pkgs;
    if cfg.linux && cfg.gui then
      [
        adwaita-qt
        arandr
        audacity
        blender
        brightnessctl
        chromium
        unfree.discord
        dolphin
        dzen2
        gimp
        glxinfo
        gnome.eog
        krita-beta
        libnotify
        # libreoffice
        lxappearance
        lxqt.lxqt-policykit
        # mangohud
        okular
        pavucontrol
        rustybar
        scrot
        signal-desktop
        unfree.slack
        vlc
        xclip
        xdotool
        xorg.xmodmap
        xorg.xrandr
        xournal
        xsel
      ]
    else
      [ ];

  default = with pkgs; [
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
    nixfmt-rfc-style
    nix-output-monitor
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
  ];
in
{
  options.custom.packages = {
    gui = mkOption {
      type = types.bool;
      default = false;
      description = "Enable gui packages";
    };

    linux = mkOption {
      type = types.bool;
      default = true;
      description = "Enable linux-only packages";
    };
  };

  config = {
    home.packages = default ++ linux ++ gui ++ linux-gui;
  };
}
