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

  orca-slicer =
    # nixpkgs is still on 2.4.2, and building 2.5.0 from source needs new
    # dependencies that aren't packaged yet, so wrap the upstream AppImage.
    # Remove once nixpkgs ships 2.5.0.
    # https://github.com/NixOS/nixpkgs/issues/585538
    let
      pname = "orca-slicer";
      version = "2.5.0";
      src = pkgs.fetchurl {
        url = "https://github.com/OrcaSlicer/OrcaSlicer/releases/download/v${version}/OrcaSlicer_Linux_AppImage_Ubuntu2404_V${version}.AppImage";
        hash = "sha256-0T6wqc1hjtCAq2uCxkwKYMRQGqyR6I1awsO1JvsUGnw=";
      };
      contents = pkgs.appimageTools.extract { inherit pname version src; };
    in
    pkgs.appimageTools.wrapType2 {
      inherit pname version src;
      extraPkgs = p: [
        p.webkitgtk_4_1
        p.libsoup_3
        p.glib-networking
        p.gst_all_1.gstreamer
        p.gst_all_1.gst-plugins-base
        p.gst_all_1.gst-plugins-good
        p.gst_all_1.gst-plugins-bad
      ];
      extraInstallCommands = ''
        install -Dm444 ${contents}/*.desktop -t $out/share/applications
        substituteInPlace $out/share/applications/*.desktop \
          --replace-fail "Exec=AppRun" "Exec=orca-slicer"
        cp -r ${contents}/usr/share/icons $out/share/
      '';
    };

  mdformat = (pkgs.mdformat.withPlugins (ps: [ ps.mdformat-gfm ]));

  guiPackages =
    if gui then
      with pkgs;
      [
        # audacity
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
        orca-slicer
        pulseaudioFull
        pwvucontrol
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
    cargo-clean-all
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
    nodejs
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
    # tabiew # broken
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
    # xan # broken :(
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
