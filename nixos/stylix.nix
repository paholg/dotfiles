{ pkgs, ... }:
{
  stylix = {
    enable = true;
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/seti.yaml";
    fonts = {
      monospace = {
        name = "Fira Code Nerd Font Mono";
        package = pkgs.nerd-fonts.fira-code;
      };
    };
    cursor = {
      package = pkgs.phinger-cursors;
      name = "phinger-cursors-dark";
      size = 24;
    };
    opacity = {
      popups = 0.7;
      terminal = 0.9;
    };
  };
}
