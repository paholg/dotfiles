{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      # beyond-identity
      gnome.seahorse
    ];
}
