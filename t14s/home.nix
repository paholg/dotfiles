{ pkgs, ... }:

{
  imports = [ ../home/common.nix ../home/gui.nix ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
  };

  programs.git.userEmail = "paho@paholg.com";

  programs.ssh.matchBlocks = {
    box = {
      hostname = "10.0.0.4";
      user = "paho";
    };
    home = {
      hostname = "174.21.77.15";
      user = "paho";
    };
  };

  programs.zsh.sessionVariables = {
    ZSH_USER_COLOR = "green";
    ZSH_HOST_COLOR = "cyan";
  };
}
