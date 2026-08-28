{ lib, pkgs, ... }:
let
  vt-autopilot = pkgs.writers.writeRubyBin "vt-autopilot" { } (builtins.readFile ./vt-autopilot.rb);
in
{
  systemd.services.vt-autopilot = {
    description = "Switch VTs between guest TV session and desktop";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-logind.service" ];
    path = [
      pkgs.kbd # chvt
      pkgs.procps # pgrep
      pkgs.systemd # systemctl
    ];
    serviceConfig = {
      ExecStart = lib.getExe vt-autopilot;
      Restart = "always";
      RestartSec = 10;
    };
  };
}
