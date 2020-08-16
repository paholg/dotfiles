{ pkgs, ... }:

{
  imports = [ ../home/common.nix ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
  };

  programs.git.userEmail = "paho@paholg.com";

  programs.zsh.sessionVariables = {
    ZSH_USER_COLOR = "blue";
    ZSH_HOST_COLOR = "magenta";
  };

  home.packages = with pkgs; [ python38Packages.tvnamer ];
}
