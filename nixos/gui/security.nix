{
  config,
  lib,
  pkgs,
  ...
}:
let
  configureService = service: {
    ${service}.rules.auth =
      let
        unixOrder = config.security.pam.services.${service}.rules.auth.unix.order;
      in
      {
        u2f.order = unixOrder + 1;
        fprintd.order = unixOrder + 2;
      };
  };

  unlocker = pkgs.writeShellApplication {
    name = "swaylock-unlock";
    runtimeInputs = with pkgs; [ coreutils ];
    text = # bash
      ''
        # We run this before XDG_RUNTIME_DIR is set :(
        RUNTIME_DIR="/run/user/$(id -u "$PAM_USER")"

        if [ -f "$RUNTIME_DIR/swaylock.pid" ]; then
          kill -s USR1 "$(cat "$RUNTIME_DIR/swaylock.pid")"
          rm "$RUNTIME_DIR/swaylock.pid"
        fi
      '';
  };
in
{
  security.pam.u2f = {
    enable = true;
    settings = {
      cue = true;
    };
  };

  environment.systemPackages = [ unlocker ];

  security.pam.services =
    lib.recursiveUpdate
      ((configureService "login") // (configureService "swaylock") // (configureService "polkit-1"))
      {
        sudo = {
          u2fAuth = false;
          fprintAuth = false;
        };

        gdm-password = {
          # GDM doesn't run the session module when switching to an existing
          # session, so w have to put this in account :(
          text = lib.mkAfter ''
            account optional ${pkgs.pam}/lib/security/pam_exec.so debug seteuid log=/tmp/swaylock-pam.log ${lib.getExe unlocker}
          '';
        };
      };
}
