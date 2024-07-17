{pkgs, ...}: {
  imports = [../../home];
  home.stateVersion = "20.09";

  custom = {
    gui = false;
    nixos = true;
    starship.host_color = "purple";
  };

  # Use the default helix so we don't have to build it.
  custom.helix.pkg = pkgs.helix;
}
