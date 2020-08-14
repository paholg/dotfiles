{ pkgs, ... }:

{
  imports = [ ../home/common.nix ../home/gui.nix ];

  programs.ssh.matchBlocks.box = {
    hostname = "10.0.0.4";
    user = "paho";
  };
}
