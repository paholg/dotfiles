{ pkgs, ... }:

{
  imports = [ ./alacritty.nix ];

  home.packages = with pkgs; [
    dialog
    dmenu
    inlyne # markdown viewer
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
}
