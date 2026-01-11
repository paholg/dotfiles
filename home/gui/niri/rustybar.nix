{ lib, pkgs, ... }:
{

  home.packages = [ pkgs.external.rustybar ];
  systemd.user.services.rustybar = {
    Unit = {
      Description = "Rustybar";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart = lib.getExe pkgs.external.rustybar;
      Restart = "always";
      Environment = [
        "RUST_BACKTRACE=1"
      ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
