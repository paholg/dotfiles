{
  gui,
  pkgs,
  ...
}:
let
  tidal-hifi =
    # tidal-hifi's in-app "disable sandbox" flag is applied too late: the
    # renderer still spawns sandboxed and aborts on /dev/shm, which shows up
    # as a grey screen on DataDome's login device check.
    # https://github.com/Mastermindzh/tidal-hifi/issues/958
    pkgs.symlinkJoin {
      name = "tidal-hifi-no-sandbox";
      paths = [ pkgs.tidal-hifi ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/tidal-hifi --add-flags "--no-sandbox"
      '';
    };

  mdformat = (pkgs.mdformat.withPlugins (ps: [ ps.mdformat-gfm ]));

  guiPackages =
    if gui then
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
        kitty.terminfo
        krita
        libnotify
        libreoffice
        lxqt.lxqt-policykit
        mesa-demos
        pavucontrol
        pulseaudioFull
        satty
        signal-desktop
        slack
        slurp
        tidal-hifi
        vlc
        wdisplays
        wl-screenrec
        wtype
        yubikey-manager
      ]
    else
      [ ];

  defaultPackages = with pkgs; [
    acpi
    argc
    ast-grep
    aws-rotate-key
    bash-language-server
    bottom
    btop-rocm
    cachix
    choose
    curl
    difftastic
    dig
    dmidecode
    dust
    dysk
    entr # Watch for file changes
    erdtree # pretty tree
    external.agenix
    external.claude-code
    external.devconcurrent
    eza
    fastfetch
    fd
    fzf
    gh # GitHub CLI
    git
    git-absorb
    htop
    hyperfine
    jc
    jnv
    jq
    just
    just-lsp
    kcov
    lazygit
    litecli # sqlite cli
    lshw
    (mdformat.withPlugins (ps: [ ps.mdformat-gfm ]))
    mkcert
    ncdu # interactive disk-usage
    net-tools
    nh # nix helper
    nil
    nix-output-monitor
    nixd
    onefetch
    openssl
    outils # sha256, etc.
    pciutils # lspci, etc.
    pgcli
    procs # ps
    psmisc # killall, fuser, etc.
    pv
    python3
    python3Packages.ruff
    ripgrep
    rnr # regex rename
    ruby_3_4
    rustup
    sccache
    sd
    serpl # search and replace
    shellcheck
    smartmontools
    socat
    sparse
    sshfs-fuse
    strace
    tabiew
    tinymist # typst language server
    tlrc # Shorter man-pages
    tokei
    tombi # toml language server
    typescript-language-server
    typst
    typstyle # typst formatter
    unzip
    usbutils
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
    home.packages = defaultPackages ++ guiPackages;
  };
}
