{ config, pkgs, ... }:
let
  lidCheck = pkgs.writeShellScript "pam-lid-check" ''
    ${pkgs.gnugrep}/bin/grep -q open /proc/acpi/button/lid/*/state
  '';

  # $PPID is the sudo process.
  markUrgent = pkgs.writeShellScript "pam-mark-urgent" ''
    echo "$PPID sudo password" | ${pkgs.netcat-openbsd}/bin/nc -UN /run/user/1000/mark-urgent.sock || true
    exit 0
  '';

  unixOrder = service: config.security.pam.services.${service}.rules.auth.unix.order;

  setOrder = service: {
    ${service}.rules.auth =
      let
        serviceUnixOrder = unixOrder service;
      in
      {
        u2f.order = serviceUnixOrder + 1;
        lid_check = {
          order = serviceUnixOrder + 2;
          control = "[success=ignore default=die]";
          modulePath = "pam_exec.so";
          args = [
            "quiet"
            "${lidCheck}"
          ];
        };
        fprintd.order = serviceUnixOrder + 3;
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

  security.pam.services = {
    login = {
      u2fAuth = false;
      fprintAuth = false;
    };
    sudo = {
      u2fAuth = false;
      fprintAuth = false;
      rules.auth.mark_urgent = {
        order = unixOrder "sudo" - 1;
        control = "optional";
        modulePath = "pam_exec.so";
        args = [
          "quiet"
          "${markUrgent}"
        ];
      };
    };
  }
  // (setOrder "swaylock")
  // (setOrder "polkit-1");
}
