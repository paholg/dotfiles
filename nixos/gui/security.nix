{ ... }:
{
  security.pam.u2f = {
    enable = true;
    settings = {
      cue = true;
    };
  };

  security.pam.services = {
    swaylock = {
      u2fAuth = true;
      fprintAuth = true;
    };
  };
}
