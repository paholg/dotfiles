{ pkgs, ... }:

{
  imports = [ ./alacritty.nix ];

  home.packages = with pkgs; [
    blender
    dialog
    dmenu
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
}
