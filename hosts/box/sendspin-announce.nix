# Announces travel boards over mDNS on their behalf, as their own
# announcements don't cross the WireGuard tunnel. See sendspin-announce.py.
{ lib, pkgs, ... }:
let
  boards = lib.filterAttrs (_: board: board.ap or null == "travel") (
    import ./home-assistant/esphome/audio-boards.nix
  );
  # Music Assistant listens for mDNS on box's LAN interface.
  interface = (import ../../router/hosts.nix).box.address;

  announce = pkgs.writers.writePython3Bin "sendspin-announce" {
    libraries = [ pkgs.python3Packages.zeroconf ];
  } (builtins.readFile ./sendspin-announce.py);
in
{
  systemd.services = lib.mapAttrs' (
    name: board:
    lib.nameValuePair "sendspin-announce-${name}" {
      description = "Announce ${board.friendlyName} over mDNS";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = "${lib.getExe announce} ${name} ${board.address} ${interface}";
        DynamicUser = true;
        Restart = "always";
        RestartSec = 5;
      };
    }
  ) boards;
}
