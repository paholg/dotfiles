{ lib, pkgs, ... }:
let
  lockIdleConf = pkgs.writeText "lock-idle.conf" ''
    general {
      ignore_dbus_inhibit = true
      ignore_systemd_inhibit = true
    }
    listener {
      timeout = 10
      on-timeout = niri msg action power-off-monitors
      ignore_inhibit = true
    }
  '';

  locker = pkgs.writeShellApplication {
    name = "locker";
    runtimeInputs = with pkgs; [
      hypridle
      swaylock
      niri
    ];
    text = # bash
      ''
        cleanup() {
          kill "$HYPR_IDLE"
        }
        trap cleanup EXIT

        hypridle -c ${lockIdleConf} &
        HYPR_IDLE=$!
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
