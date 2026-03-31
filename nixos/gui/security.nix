{ config, pkgs, ... }:
let
  lidCheck = pkgs.writeShellScript "pam-lid-check" ''
    ${pkgs.gnugrep}/bin/grep -q open /proc/acpi/button/lid/*/state
  '';
  setOrder = service: {
    ${service}.rules.auth =
      let
        unixOrder = config.security.pam.services.${service}.rules.auth.unix.order;
      in
      {
        u2f.order = unixOrder + 1;
        lid_check = {
          order = unixOrder + 2;
          control = "[success=ignore default=die]";
          modulePath = "pam_exec.so";
          args = [ "quiet" "${lidCheck}" ];
        };
        fprintd.order = unixOrder + 3;
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
