{ lib, pkgs, ... }:
let
  niri-notify-urgent = pkgs.writeShellApplication {
    name = "niri-notify-urgent";
    runtimeInputs = with pkgs; [
      jq
      systemd # busctl
    ];
    text = builtins.readFile ./niri-notify-urgent.sh;
  };
in
{
  systemd.user.services.niri-notify-urgent = {
    Unit = {
      Description = "Mark niri workspaces urgent on notification";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart = lib.getExe niri-notify-urgent;
      Restart = "always";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
