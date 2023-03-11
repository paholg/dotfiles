{ pkgs, ... }:

{
  home.packages = with pkgs;
    [
      cargo-insta
    ];
}
