{ ... }:
{
  imports = [ ../../home ];
  home.stateVersion = "20.09";

  custom = {
    username = "paho";

    starship.host_color = "purple";
  };
}
