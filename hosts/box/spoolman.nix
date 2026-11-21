# Spoolman: filament spool inventory.
#
# Web access is through nginx + oauth2-proxy (see nginx.nix). Spoolman has no
# auth of its own, so the port is otherwise only reachable by the Snapmaker U1,
# whose Moonraker talks to the API directly:
# https://moonraker.readthedocs.io/en/latest/configuration/#spoolman
{ config, lib, ... }:
let
  port = config.custom.ports.spoolman;
  printer = (import ../../router/hosts.nix).snapmaker-u1.address;
  dataDir = config.custom.drives.data + "/spoolman";
in
{
  users.groups.spoolman.gid = config.custom.groups.spoolman;
  users.users.spoolman = {
    isSystemUser = true;
    group = "spoolman";
    uid = config.custom.uids.spoolman;
  };

  services.spoolman = {
    enable = true;
    listen = "0.0.0.0";
    inherit port;
    environment = {
      SPOOLMAN_DIR_DATA = dataDir;
      SPOOLMAN_AUTOMATIC_BACKUP = "TRUE";
    };
  };

  # The module uses DynamicUser with /var/lib/spoolman; use a static user so
  # the data can live with everything else.
  systemd.tmpfiles.rules = [ "d ${dataDir} 0750 spoolman spoolman -" ];
  systemd.services.spoolman = {
    unitConfig.RequiresMountsFor = dataDir;
    serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "spoolman";
      Group = "spoolman";
    };
  };

  networking.firewall.extraCommands = ''
    iptables -A nixos-fw -p tcp -s ${printer} --dport ${toString port} -j nixos-fw-accept
  '';
}
