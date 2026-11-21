# Spoolman: filament spool inventory.
#
# Spoolman has no auth of its own, so it only listens on localhost; all access,
# including the Snapmaker U1's Moonraker, is through nginx (see nginx.nix).
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
    listen = "127.0.0.1";
    inherit port;
    environment = {
      SPOOLMAN_DIR_DATA = dataDir;
      SPOOLMAN_AUTOMATIC_BACKUP = "TRUE";
      # Fluidd opens a websocket to Spoolman straight from the browser, which
      # Spoolman refuses unless it trusts the page's origin.
      SPOOLMAN_CORS_ORIGIN = "https://u1.paholg.com,http://${printer}";
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
}
