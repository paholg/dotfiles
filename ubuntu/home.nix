{ pkgs, ... }:

{
  imports = [
    ../home/common.nix
    ../home/common-linux.nix
    ../home/gui.nix
    ../home/firefox.nix
    ../home/packages-gui.nix
    ../home/packages-gui-linux.nix
  ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
    sessionVariables = {
      GOPATH = "$HOME/go";
      ZEROPW = "$GOPATH/src/gitlab.com/zeropw/zero";
      AUTHN = "$HOME/bi/authn";
      AWS_PROFILE = "development";
      CARGO_REGISTRY_AUTH_URL = "$(cat $HOME/.git-credentials)";
      GONOSUMDB = "go.beyondidentity.com/*";
      GOPROXY = "$(cat $HOME/.goproxy)";
      DATABASE_URL =
        "postgresql://postgres:beyondidentity@dockerhost:5435/postgres?sslmode=disable";
    };
  };

  programs.git = {
    userEmail = "paho@paholg.com";
    signing = {
      gpgPath = "/opt/beyond-identity/bin/gpg-bi";
      key = "5F78989E28A4FC0D7D507176AE9E04831E891B04";
      signByDefault = true;
    };

    includes = [{
      condition = "gitdir:~/bi/";
      contents.user.email = "paho.lurie-gregg@beyondidentity.com";
    }];
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
    jetbrains.idea-community
    k9s
    kops
    kubectl
    kubernetes-helm
    mercurial
    minikube
    # nodejs
    openapi-generator-cli
    openjdk8
    python39Packages.swagger-spec-validator
    python39Packages.swagger-ui-bundle
    python310Packages.openapi-spec-validator
    qemu_full
    redoc-cli
    remmina
    sqlitebrowser
    sqlx-cli
    tpm2-tss
    wineWowPackages.stable
    yarn
    yubikey-manager
  ];
}
