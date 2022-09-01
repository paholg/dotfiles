{ pkgs, ... }:

{
  imports = [
    ../home/common.nix
    ../home/common-linux.nix
    ../home/firefox.nix
    ../home/gui.nix
    ../home/packages-gui-linux.nix
    ../home/packages-bi.nix
  ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";

    keyboard.options = [ "caps:backspace" ];
  };

  programs.git.userEmail = "paho@paholg.com";

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

  home.packages = with pkgs; [ glibc vulkan-tools yubikey-manager ];
}
