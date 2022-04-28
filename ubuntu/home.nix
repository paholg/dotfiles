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
        "postgres://postgres:authnpw@dockerhost:8015/postgres?sslmode=disable";
    };
  };

  programs.git = {
    userEmail = "paho.lurie-gregg@beyondidentity.com";
    signing = {
      gpgPath = "/opt/beyond-identity/bin/gpg-bi";
      key = "0A0862F72BBF7DDBEC6DCB6A8B7CC784EDC9150D";
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
    sqlitebrowser
    sqlx-cli
    tpm2-tss
    yarn
  ];
}
