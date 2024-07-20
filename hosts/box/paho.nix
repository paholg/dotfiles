{ ... }:
{
  imports = [ ../../home ];
  home.stateVersion = "20.09";

  custom = {
    gui = false;
    linux = true;
    nixos = true;
    # wayland = false;
    # x11 = false;
    username = "paho";

    starship.host_color = "purple";
  };
}
