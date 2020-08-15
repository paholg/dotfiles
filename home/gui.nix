{ pkgs, ... }:

{
  imports = [ ./alacritty.nix ];

  home.packages = with pkgs; [ physlock ];

  # programs.firefox = {
  #   enable = true;
  # };

  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
      temperature = {
        day = 5000;
        night = 3300;
      };
    };
  };

  xsession = {
    enable = true;

    initExtra = ''
      monitor_switch default &

      fixkb &

      # Set cursor
      xsetroot -cursor_name left_ptr &

      # startup programs
      background 150 &

      # Load resources
      xrdb -merge .Xresources &
    '';

    windowManager = {
      xmonad = {
        enable = true;
        config = ./xmonad.hs;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
        ];
      };
    };
  };
}
