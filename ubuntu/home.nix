{ pkgs, ... }:

{
  imports = [
    ../home/common.nix
    ../home/common-linux.nix
    ../home/gui.nix
    ../home/packages-gui.nix
    ../home/packages-gui-linux.nix
  ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
    sessionVariables = {
      GOPATH = "$HOME/go";
      ZEROPW = "$GOPATH/src/gitlab.com/zeropw/zero";
      AWS_PROFILE = "development";
      CARGO_REGISTRY_AUTH_URL = "$(cat $HOME/.git-credentials)";
      GONOSUMDB = "go.beyondidentity.com/*";
      GOPROXY = "$(cat $HOME/.goproxy)";
      DATABASE_URL =
        "postgresql://postgres:beyondidentity@dockerhost:5435/postgres?sslmode=disable";
    };
  };

  programs.git = {
    userEmail = "paho.lurie-gregg@beyondidentity.com";
    signing = {
      gpgPath = "/opt/beyond-identity/bin/gpg-bi";
      key = "64B10D69F869D49CA4D716B817A44143A74D6094";
      signByDefault = true;
    };
  };

  programs.ssh.matchBlocks = { };

  programs.zsh.shellAliases = { };

  home.packages = with pkgs; [
    awscli2
    docker
    docker-compose
    fpm
    go
    gopls
    mercurial
    nodejs
    openapi-generator-cli
    python39Packages.swagger-spec-validator
    python39Packages.swagger-ui-bundle
    python310Packages.openapi-spec-validator
    redoc-cli
    remmina
    sqlitebrowser
    sqlx-cli
    tpm2-tss
    yarn
  ];
}
