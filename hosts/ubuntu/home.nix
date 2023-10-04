{pkgs, ...}: let
  database_url = "postgresql://postgres:beyondidentity@dockerhost:5435/postgres?sslmode=disable";
in {
  imports = [
    ../../home/common.nix
    ../../home/common-linux.nix
    ../../home/gui.nix
    ../../home/firefox.nix
    ../../home/packages-bi.nix
    ../../home/packages-gui.nix
    ../../home/packages-gui-linux.nix
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
      DATABASE_URL = database_url;
    };
  };

  programs.git = {
    userEmail = "paho@paholg.com";
    signing = {
      gpgPath = "/opt/beyond-identity/bin/gpg-bi";
      key = "054F308A6202814D4F1629C317878DA9DD6CEA7C";
      signByDefault = true;
    };

    includes = [
      {
        condition = "gitdir:~/bi/";
        contents.user.email = "paho.lurie-gregg@beyondidentity.com";
      }
    ];
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

  programs.zsh.shellAliases = {
    vpn = "'/opt/awsvpnclient/AWS VPN Client'";
  };

  # compositing for zoom
  services.picom.enable = true;

  systemd.user.sessionVariables = {
    DATABASE_URL = database_url;
  };

  home.packages = with pkgs; [
    awscli2
    argo
    cloudsmith-cli
    cmake
    # docker
    # docker-compose
    fpm
    gnome.seahorse
    go
    gopls
    jetbrains.idea-community
    k9s
    kops
    kubectl
    kubernetes-helm
    lldb
    mercurial
    minikube
    # nodejs
    openapi-generator-cli
    openjdk8
    # podman
    # podman-compose
    # python39Packages.swagger-spec-validator
    # python39Packages.swagger-ui-bundle
    python310Packages.openapi-spec-validator
    qemu_full
    redoc-cli
    remmina
    sqlitebrowser
    sqlx-cli
    # swift
    tpm2-tss
    valgrind
    wineWowPackages.stable
    yubikey-manager
  ];
}
