{ pkgs, ... }:

{
  imports = [ ./alacritty.nix ];

  home.packages = with pkgs; [
    dialog
    dmenu
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
}
