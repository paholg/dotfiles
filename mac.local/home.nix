{ pkgs, ... }:

{
  imports = [ ../home/common.nix ../home/packages-gui.nix ];

  home = {
    username = "paho";
    homeDirectory = "/Users/paho";
  };

  programs.git.userEmail = "paho.lurie-gregg@beyondidentity.com";

  home.packages = with pkgs; [ awscli2 go mercurial yarn ];
}
