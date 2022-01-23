{ pkgs, ... }:

{
  imports = [ ../home/common.nix ../home/common-linux.nix ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
  };

  home.file = {
    ".config/tvnamer/tvnamer.json".text = (builtins.readFile ./tvnamer.json);
  };

  programs.git.userEmail = "paho@paholg.com";

  home.packages = with pkgs; [ tvnamer ];
}
