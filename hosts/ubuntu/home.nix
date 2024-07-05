{ pkgs, ... }:
let
  database_url = "postgresql://postgres:beyondidentity@dockerhost:5435/postgres?sslmode=disable";
  shellAliases = {
    vpn = "'/opt/awsvpnclient/AWS VPN Client'";
  };

  bi_git = {
    commit.gpgSign = true;
    gpg.program = "/opt/beyond-identity/bin/gpg-bi";

    user.email = "paho.lurie-gregg@beyondidentity.com";
    user.signingKey = "DC50592397AF3F8EEAD25A8522EF27F29CB66537";
  };
in
{
  home.stateVersion = "20.09";

  custom.home = {
    gui = true;
    nixos = false;
  };
  custom.xmonad.enable = true;

  home = {
    sessionVariables = {
      GOPATH = "$HOME/go";
      ZEROPW = "$GOPATH/src/gitlab.com/zeropw/zero";
      AUTHN = "$HOME/bi/authn";
      AWS_PROFILE = "development";
      CARGO_REGISTRY_AUTH_URL = "$(cat $HOME/.git-credentials)";
      GONOSUMDB = "go.beyondidentity.com/*";
      GOPROXY = "$(cat $HOME/.goproxy)";
      DATABASE_URL = database_url;
      CREDENTIALS_SECRET_KEY_PATH = "$ZEROPW/services/mdm-go/local-secret-key/secret.key";

      # Get a login-popup error without this.
      SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh";
    };
  };

  custom.display-switch = {
    enable = true;
    settings = {
      globalSection = {
        usb_device = "046d:c52b";
      };
      sections = {
        monitor1 = {
          monitor_id = "Gigabyte M32U";
          on_usb_connect = "DisplayPort2";
          on_usb_disconnect = "DisplayPort1";
        };
        monitor2 = {
          monitor_id = "HP Z32";
          on_usb_connect = "HDMI1";
          on_usb_disconnect = "DisplayPort1";
        };
      };
    };
  };

  programs.git = {
    includes = [
      {
        condition = "gitdir:~/bi/";
        contents = bi_git;
      }
      {
        condition = "gitdir:~/go/src/gitlab.com/zeropw/";
        contents = bi_git;
      }
    ];
  };

  programs.fish.shellAliases = shellAliases;

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
    # jetbrains.idea-community
    k9s
    kops
    kubectl
    kubernetes-helm
    lldb
    # mercurial
    minikube
    # nodejs
    openapi-generator-cli
    openjdk8
    # podman
    # podman-compose
    # python39Packages.swagger-spec-validator
    # python39Packages.swagger-ui-bundle
    python310Packages.openapi-spec-validator
    # qemu_full
    # redoc-cli
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
