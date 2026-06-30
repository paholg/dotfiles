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
          args = [
            "quiet"
            "${lidCheck}"
          ];
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

  security.pam.services = {
    login = {
      u2fAuth = false;
      fprintAuth = false;
    };
    sudo = {
      u2fAuth = false;
      fprintAuth = false;
      rules.auth.mark_urgent = {
        order = config.security.pam.services.sudo.rules.auth.unix.order - 1;
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
