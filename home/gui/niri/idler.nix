{ lib, pkgs, ... }:
let
  # Lock the session, keeping displays off.
  locker = pkgs.writeShellApplication {
    name = "locker";
    runtimeInputs = with pkgs; [
      swayidle
      swaylock
      niri
    ];
    text = # bash
      ''
        cleanup() {
          kill "$SWAY_IDLE"
        }
        trap cleanup EXIT

        swayidle timeout 10 'niri msg action power-off-monitors' &
        SWAY_IDLE=$!
        swaylock
      '';
  };
  idler = pkgs.writeShellApplication {
    name = "idler";
    runtimeInputs = with pkgs; [
      swayidle
      locker
    ];
    text = "swayidle timeout 300 'locker'";
  };
in
{
  home.packages = [ locker ];

  systemd.user.services.idler = {
    Unit = {
      Description = "Idler";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart = lib.getExe idler;
      Restart = "always";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
