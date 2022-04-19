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
        "postgres://localhost?user=postgres&password=postgres&dbname=authn";
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

  programs.zsh.shellAliases = {
    ns = ''
      function _ns() { nix-shell -p pkgconfig openssl tpm2-tss sqlite --run "$*" }; _ns'';
    c = ''function _c() { ns "cargo "$*"" }; _c'';
    ccheck = ''
      function _ccheck() { ns "cargo check --color always "$*" 2>&1 | bat" }; _ccheck'';
    ctest = ''
      function _ctest() { ns "cargo test --color always "$*" 2>&1 | bat" }; _ctest'';
  };

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
