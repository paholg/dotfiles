{ ... }:
{
  imports = [
    ../../home
    ./display_switch.nix
  ];
  home.stateVersion = "24.05";

  custom.home = {
    username = "guest";
    gui = true;
    nixos = true;
    sway_tty = 2;
  };
  custom.sway = {
    enable = true;
    startup = [
      { command = "steam-gamescope"; }
      { command = "firefox"; }
    ];
    extraConfig = [
      ''output HDMI-A-1 enable pos 0 0 mode 3840x2160@60Hz bg "#000000" solid_color''
      "output DP-2 disable"
      "output DP-3 disable"
      "workspace 1"
    ];
  };
  custom.starship.host_color = "cyan";

  # We never want to blank or lock screen for this user.
  services.swayidle.enable = false;
}
