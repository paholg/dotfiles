{ pkgs, ... }:

{
  home.packages = with pkgs;
    let
      my-python-packages = python-packages:
        with python-packages; [
          numpy
          black
          torch
        ];
      python-with-my-packages = python3.withPackages my-python-packages;
    in
    [
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
      cargo-deny
      cargo-expand
      cargo-outdated
      cargo-update
      cargo-watch
      choose
      # clang
      curl
      difftastic
      direnv
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
      jq
      jwt-cli
      mdcat
      mold
      neofetch
      niv
      nixfmt
      nmap
      nodePackages.bash-language-server
      nodePackages.typescript-language-server
      openssl
      pciutils # lspci, etc.
      perl
      # pkg-config
      pgcli
      # python-with-my-packages
      # python310Packages.python-lsp-server
      ripgrep
      rnix-lsp
      ruby
      rust-analyzer
      rustup
      sd
      shellcheck
      socat
      solargraph
      sshfs-fuse
      # swift
      taplo-cli # toml language server
      terraform-ls
      tokei
      unison
      vim
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
