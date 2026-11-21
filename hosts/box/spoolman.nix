# Spoolman: filament spool inventory.
#
# Web access is through nginx + oauth2-proxy (see nginx.nix). Spoolman has no
# auth of its own, so the port is otherwise only reachable by the Snapmaker U1,
# whose Moonraker talks to the API directly:
# https://moonraker.readthedocs.io/en/latest/configuration/#spoolman
{ config, ... }:
let
  port = config.custom.ports.spoolman;
  printer = (import ../../router/hosts.nix).snapmaker-u1.address;
in
{
  services.spoolman = {
    enable = true;
    listen = "0.0.0.0";
    inherit port;
    environment.SPOOLMAN_AUTOMATIC_BACKUP = "TRUE";
  };

  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -p tcp -s ${printer} --dport ${toString port} -j nixos-fw-accept
  '';
}
