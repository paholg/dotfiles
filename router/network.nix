{ lib, ... }:
let
  hosts = import ./hosts.nix;

  bridge = "\${routeros_interface_bridge.bridge.name}";
  dhcpServer = "\${routeros_ip_dhcp_server.dhcp-server.name}";
  lanList = "\${routeros_interface_list.LAN.name}";
  wanList = "\${routeros_interface_list.WAN.name}";

  dnsServers = [
    "9.9.9.9"
    "149.112.112.112"
  ];

  # Losing any of these would cut off access to the router.
  protect.lifecycle.prevent_destroy = true;

  bridgePorts = [
    "ether2"
    "ether3"
    "ether4"
    "ether5"
    "ether6"
    "ether7"
    "ether8"
    "sfp-sfpplus1"
  ];

  hostLeases = lib.mapAttrs (
    name: host:
    {
      address = host.address;
      comment = host.comment or name;
      mac_address = host.mac;
      server = dhcpServer;
    }
    // lib.optionalAttrs (host ? clientId) { client_id = host.clientId; }
  ) hosts;

  apLease = ap: address: {
    inherit address;
    comment = "wifi-${ap}";
    mac_address = "\${local.private.ap_macs.${ap}}";
    server = dhcpServer;
  };
in
{
  resource = {
    routeros_interface_bridge.bridge = {
      name = "bridge";
      admin_mac = "\${local.private.admin_mac}";
      auto_mac = false;
      comment = "defconf";
    }
    // protect;

    routeros_interface_bridge_port = lib.genAttrs bridgePorts (interface: {
      inherit bridge interface;
      comment = "defconf";
    }
    // protect);

    routeros_interface_list = {
      WAN = {
        name = "WAN";
        comment = "defconf";
      }
      // protect;
      LAN = {
        name = "LAN";
        comment = "defconf";
      }
      // protect;
    };

    routeros_interface_list_member = {
      bridge-lan = {
        interface = bridge;
        list = lanList;
        comment = "defconf";
      };
      ether1-wan = {
        interface = "ether1";
        list = wanList;
      };
    };

    routeros_ip_address.lan = {
      address = "10.0.0.1/16";
      interface = bridge;
    }
    // protect;

    routeros_ip_dhcp_client.wan = {
      interface = "ether1";
      comment = "defconf";
    }
    // protect;

    routeros_ip_pool.dhcp-pool = {
      name = "dhcp-pool";
      ranges = [ "10.0.100.1-10.0.100.254" ];
    };

    routeros_ip_dhcp_server.dhcp-server = {
      name = "dhcp-server";
      address_pool = "\${routeros_ip_pool.dhcp-pool.name}";
      interface = bridge;
    };

    routeros_ip_dhcp_server_network.lan = {
      address = "10.0.0.0/16";
      dns_server = dnsServers;
      gateway = "10.0.0.1";
    };

    routeros_ip_dhcp_server_lease = hostLeases // {
      wifi-up = apLease "up" "10.0.1.1";
      wifi-main = apLease "main" "10.0.1.2";
      wifi-garage = apLease "garage" "10.0.1.3";
    };

    routeros_ip_dns.dns = {
      allow_remote_requests = true;
      servers = dnsServers;
      use_doh_server = "https://dns.quad9.net/dns-query";
    };

    routeros_ip_dns_record."router-lan" = {
      name = "router.lan";
      address = "10.0.0.1";
      type = "A";
      comment = "defconf";
    };
  };
}
