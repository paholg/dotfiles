{ pkgs, ... }:

{
  home.packages = with pkgs;
    let
      my-python-packages = python-packages:
        with python-packages; [
          numpy
          black
        ];
      python-with-my-packages = python3.withPackages my-python-packages;
    in [
      angle-grinder
      arandr
      aws-rotate-key
      bat
      bat-extras.batgrep
      bat-extras.batman
      bat-extras.prettybat
      bind
      # binutils
      bitwarden-cli
      bottom
      broot
      cargo-audit
      cargo-edit
      cargo-expand
      cargo-insta
      cargo-outdated
      cargo-update
      cargo-watch
      choose
      clang
      difftastic
      diskus
      du-dust
      exa
      fd
      feh
      fzf
      git
      gitAndTools.delta
      gitAndTools.hub
      gnuplot
      haskell-language-server
      htop
      httpie
      ion
      jq
      mdcat
      mold
      niv
      nixfmt
      nmap
      nodePackages.bash-language-server
      nodePackages.typescript-language-server
      openssl
      pciutils # lspci, etc.
      perl
      pkg-config
      pgcli
      python-with-my-packages
      python310Packages.python-lsp-server
      ripgrep
      rnix-lsp
      ruby
      rust-analyzer
      rustup
      sd
      socat
      solargraph
      sshfs-fuse
      # swift
      tokei
      unison
      vim
      wally-cli
      wget
      zenith
      zip
      zsh
    ];
}
