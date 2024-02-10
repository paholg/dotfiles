{pkgs, ...}: {
  imports = [
    ../../home/common.nix
    ../../home/common-linux.nix
    ../../home/gui.nix
    # ../../home/firefox.nix
    ../../home/packages-gui.nix
    ../../home/packages-gui-linux.nix
    ./hyprland.nix
  ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";
  };

  programs.git.userEmail = "paho@paholg.com";

  home.packages = with pkgs; [
    firefox
  ];
}
