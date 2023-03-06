{ pkgs, lib, ... }:

{
  imports = [ ../home/common.nix ../home/packages-gui.nix ];

  home = {
    username = "paho";
    homeDirectory = "/Users/paho";
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

  programs.git.userEmail = "paho.lurie-gregg@beyondidentity.com";

  programs.alacritty.settings.font.size = lib.mkForce 20.0;

  home.packages = with pkgs; [ awscli2 go yarn ];
}
