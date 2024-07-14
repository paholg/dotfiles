{ ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
  ];
  home.stateVersion = "20.09";

  custom.home = {
    gui = true;
    nixos = true;
  };
  custom.sway = {
    enable = true;
    startup = [
      { command = "steam-gamescope"; }
      { command = "firefox"; }
      { command = "Discord"; }
    ];
    extraConfig = [
      "output HDMI-A-1 disable"
      ''output DP-2 enable pos 2160 0 mode 3840x2160@60Hz transform 90 bg "#000000" solid_color''
      ''output DP-3 enable pos 0 0 mode 3840x2160@144Hz transform 270 bg "#000000" solid_color''
    ];
  };
  custom.starship.host_color = "cyan";
}
