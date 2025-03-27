{ pkgs, ... }:
{
  fonts.packages = with pkgs; [
    nerd-fonts.fira-code
    nerd-fonts.monaspace
    monaspace
  ];

  stylix = {
    enable = true;
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/seti.yaml";
    fonts = {
      monospace = {
        name = "Monaspace Neon";
        package = pkgs.nerd-fonts.monaspace;
      };
    };
    cursor = {
      package = pkgs.phinger-cursors;
      name = "phinger-cursors-light";
      size = 32;
    };
    opacity = {
      popups = 0.9;
      terminal = 0.9;
    };
  };
}
