{pkgs, ...}: let
  database_url = "postgresql://postgres:beyondidentity@dockerhost:5435/postgres?sslmode=disable";
  shellAliases = {
    vpn = "'/opt/awsvpnclient/AWS VPN Client'";
  };
in {
  imports = [
    ../../home/common.nix
    ../../home/common-linux.nix
    ../../home/gui.nix
    ../../home/xmonad.nix
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
      CREDENTIALS_SECRET_KEY_PATH="$ZEROPW/services/mdm-go/local-secret-key/secret.key";
  };

  programs.git = {
    userEmail = "paho@paholg.com";
    signing = {
      gpgPath = "/opt/beyond-identity/bin/gpg-bi";
      key = "67D6EBC88CB058D464CD4F78046380E0B8566EE7";
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

  programs.fish.shellAliases = shellAliases;
  programs.zsh.shellAliases = shellAliases;

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
