{pkgs, ...}: {
  imports = [../../home/common.nix ../../home/common-linux.nix];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";

    packages = with pkgs; [transmission];
  };

  programs.git.userEmail = "paho@paholg.com";

  programs.zsh.enable = true;
}
