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
      key = "1DAFB2E1FCF44BB958273C9A778DAE049683100E";
      signByDefault = true;
    };

    includes = [{
      condition = "gitdir:~/bi/";
      contents.user.email = "paho.lurie-gregg@beyondidentity.com";
    }];
  };
  programs.zsh.shellAliases = {
    docker = "podman";
    # docker-compose = "podman-compose";
  };

  programs.ssh.matchBlocks = {
    box = {
      hostname = "10.0.0.4";
      user = "paho";
    };

    home = {
      hostname = "home.paholg.com";
      user = "paho";
    };
  };

  programs.zsh.shellAliases = { };

  # compositing for zoom
  services.picom.enable = true;

  home.packages = with pkgs; [
    awscli2
    argo
    cloudsmith-cli
    cmake
    # docker
    # docker-compose
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
    # podman
    # podman-compose
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
    yubikey-manager
  ];
}
