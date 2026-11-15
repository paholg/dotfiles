# WireGuard server for remote sites. Peers route their LAN here, so services on
# box (like Music Assistant) can reach devices behind them.
#
# Not named wg0, which vpn.nix creates before moving it into its namespace.
#
# https://wiki.nixos.org/wiki/WireGuard
# https://search.nixos.org/options?query=networking.wireguard.interfaces
{ config, ... }:
let
  port = config.custom.ports.wireguard_travel;
in
{
  age.secrets.box_wireguard_private_key.file = ../../secrets/box_wireguard_private_key;

  networking.firewall.allowedUDPPorts = [ port ];

  networking.wireguard.interfaces.wg-travel = {
    ips = [ "10.100.0.1/24" ];
    listenPort = port;
    privateKeyFile = config.age.secrets.box_wireguard_private_key.path;
    # Leaves room for the outer headers on hotel networks with small MTUs.
    mtu = 1280;
    # Each peer's allowedIPs are also added as routes (allowedIPsAsRoutes).
    peers = [
      {
        name = "opal";
        publicKey = "/YHDHFUC4m+xaba3bAdgYvLzk6HbF7huB8XZxfeaChU=";
        allowedIPs = [
          "10.100.0.2/32"
          # The Opal's LAN; it must not masquerade it, so box can connect in.
          "192.168.8.0/24"
        ];
      }
    ];
  };
}
