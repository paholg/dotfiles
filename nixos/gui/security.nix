{ config, ... }:
let
  setOrder = service: {
    ${service}.rules.auth =
      let
        unixOrder = config.security.pam.services.${service}.rules.auth.unix.order;
      in
      {
        u2f.order = unixOrder + 1;
        fprintd.order = unixOrder + 2;
      };
  };
in
{
  security.pam.u2f = {
    enable = true;
    settings = {
      cue = true;
    };
  };

  security.pam.services =
    {
      login = {
        u2fAuth = false;
        fprintAuth = false;
      };
      sudo = {
        u2fAuth = false;
        fprintAuth = false;
      };
    }
    // (setOrder "swaylock")
    // (setOrder "polkit-1");
}
