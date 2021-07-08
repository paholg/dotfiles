{ pkgs, ... }:

{
  imports = [ ../home/common.nix ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
  };

  home.file = {
    ".config/tvnamer/tvnamer.json".text = (builtins.readFile ./tvnamer.json);
  };

  programs.git.userEmail = "paho@paholg.com";

  programs.zsh.sessionVariables = {
    ZSH_USER_COLOR = "blue";
    ZSH_HOST_COLOR = "magenta";
  };

  home.packages = with pkgs; [ tvnamer ];
}
