{ ... }:
{
  imports = [
    ../../home
    (import ../../home/gui/niri/mkConfig.nix [
      "base.kdl"
      "binds.kdl"
      "guest.kdl"
    ])
  ];
  home.stateVersion = "24.05";

  custom = {
    username = "guest";
    mangohud.enable = false;
    starship.host_color = "cyan";
  };
}
