{ pkgs, ... }:

{
  imports = [
    ../home/common.nix
    ../home/common-linux.nix
    ../home/firefox.nix
    ../home/gui.nix
    ../home/packages-gui-linux.nix
  ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
  };

  programs.git.userEmail = "paho.lurie-gregg@beyondidentity.com";

  home.packages = with pkgs; [ awscli2 ];
}
