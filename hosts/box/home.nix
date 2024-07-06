{ ... }:
{
  imports = [ ../../home ];
  home.stateVersion = "20.09";

  custom.home = {
    gui = false;
    nixos = true;
  };
  custom.starship.host_color = "purple";

  # Use the default helix so we don't have to build it here.
  custom.helix.pkg = "helix";
}
