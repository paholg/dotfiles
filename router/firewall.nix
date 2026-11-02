{ lib, ... }:
let
  hosts = import ./hosts.nix;

  established = "established,related,untracked";

  toResources = rules: lib.listToAttrs (map (rule: lib.nameValuePair rule.name (removeAttrs rule [ "name" ])) rules);

  # Moves all rules but the last before the last, which leaves them in list order.
  ordered = type: path: rules: {
    resource_path = path;
    sequence = map (rule: "\${${type}.${rule.name}.id}") rules;
  };

  forwardToBox = service: port: {
    name = "box-${service}";
    action = "dst-nat";
    chain = "dstnat";
    comment = "box - ${service}";
    dst_address = "!10.0.0.1";
    dst_address_type = "local";
    dst_port = toString port;
    protocol = "tcp";
    to_addresses = hosts.box.address;
  };

  filter = [
    {
      name = "input-accept-established";
      action = "accept";
      chain = "input";
      comment = "defconf: accept established,related,untracked";
      connection_state = established;
    }
    {
      name = "input-drop-invalid";
      action = "drop";
      chain = "input";
      comment = "defconf: drop invalid";
      connection_state = "invalid";
    }
    {
      name = "input-accept-icmp";
      action = "accept";
      chain = "input";
      comment = "defconf: accept ICMP";
      protocol = "icmp";
    }
    {
      name = "input-accept-loopback";
      action = "accept";
      chain = "input";
      comment = "defconf: accept to local loopback (for CAPsMAN)";
      dst_address = "127.0.0.1";
    }
    {
      name = "input-drop-not-lan";
      action = "drop";
      chain = "input";
      comment = "defconf: drop all not coming from LAN";
      in_interface_list = "!LAN";
    }
    {
      name = "forward-accept-ipsec-in";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept in ipsec policy";
      ipsec_policy = "in,ipsec";
    }
    {
      name = "forward-accept-ipsec-out";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept out ipsec policy";
      ipsec_policy = "out,ipsec";
    }
    {
      name = "forward-fasttrack";
      action = "fasttrack-connection";
      chain = "forward";
      comment = "defconf: fasttrack";
      connection_state = "established,related";
      hw_offload = true;
    }
    {
      name = "forward-accept-established";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept established,related, untracked";
      connection_state = established;
    }
    {
      name = "forward-drop-invalid";
      action = "drop";
      chain = "forward";
      comment = "defconf: drop invalid";
      connection_state = "invalid";
    }
    {
      name = "forward-drop-wan-not-dstnat";
      action = "drop";
      chain = "forward";
      comment = "defconf: drop all from WAN not DSTNATed";
      connection_nat_state = "!dstnat";
      connection_state = "new";
      in_interface_list = "WAN";
    }
  ];

  nat = [
    {
      name = "hairpin";
      action = "masquerade";
      chain = "srcnat";
      comment = "Hairpin NAT";
      dst_address = "10.0.0.0/16";
      src_address = "10.0.0.0/16";
    }
    {
      name = "masquerade";
      action = "masquerade";
      chain = "srcnat";
      comment = "defconf: masquerade";
      ipsec_policy = "out,none";
      out_interface_list = "WAN";
    }
    (forwardToBox "ssh" 22)
    (forwardToBox "http" 80)
    (forwardToBox "https" 443)
    (forwardToBox "plex" 32400)
  ];

  badIpv6 = {
    unspecified = {
      address = "::/128";
      comment = "defconf: unspecified address";
    };
    lo = {
      address = "::1/128";
      comment = "defconf: lo";
    };
    site-local = {
      address = "fec0::/10";
      comment = "defconf: site-local";
    };
    ipv4-mapped = {
      address = "::ffff:0.0.0.0/96";
      comment = "defconf: ipv4-mapped";
    };
    ipv4-compat = {
      address = "::/96";
      comment = "defconf: ipv4 compat";
    };
    discard-only = {
      address = "100::/64";
      comment = "defconf: discard only ";
    };
    documentation = {
      address = "2001:db8::/32";
      comment = "defconf: documentation";
    };
    orchid = {
      address = "2001:10::/28";
      comment = "defconf: ORCHID";
    };
    sixbone = {
      address = "3ffe::/16";
      comment = "defconf: 6bone";
    };
  };

  filter6 = [
    {
      name = "input-accept-established";
      action = "accept";
      chain = "input";
      comment = "defconf: accept established,related,untracked";
      connection_state = established;
    }
    {
      name = "input-drop-invalid";
      action = "drop";
      chain = "input";
      comment = "defconf: drop invalid";
      connection_state = "invalid";
    }
    {
      name = "input-accept-icmpv6";
      action = "accept";
      chain = "input";
      comment = "defconf: accept ICMPv6";
      protocol = "icmpv6";
    }
    {
      name = "input-accept-traceroute";
      action = "accept";
      chain = "input";
      comment = "defconf: accept UDP traceroute";
      dst_port = "33434-33534";
      protocol = "udp";
    }
    {
      name = "input-accept-dhcpv6-pd";
      action = "accept";
      chain = "input";
      comment = "defconf: accept DHCPv6-Client prefix delegation.";
      dst_port = "546";
      protocol = "udp";
      src_address = "fe80::/10";
    }
    {
      name = "input-accept-ike";
      action = "accept";
      chain = "input";
      comment = "defconf: accept IKE";
      dst_port = "500,4500";
      protocol = "udp";
    }
    {
      name = "input-accept-ipsec-ah";
      action = "accept";
      chain = "input";
      comment = "defconf: accept ipsec AH";
      protocol = "ipsec-ah";
    }
    {
      name = "input-accept-ipsec-esp";
      action = "accept";
      chain = "input";
      comment = "defconf: accept ipsec ESP";
      protocol = "ipsec-esp";
    }
    {
      name = "input-accept-ipsec-policy";
      action = "accept";
      chain = "input";
      comment = "defconf: accept all that matches ipsec policy";
      ipsec_policy = "in,ipsec";
    }
    {
      name = "input-drop-not-lan";
      action = "drop";
      chain = "input";
      comment = "defconf: drop everything else not coming from LAN";
      in_interface_list = "!LAN";
    }
    {
      name = "forward-accept-established";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept established,related,untracked";
      connection_state = established;
    }
    {
      name = "forward-drop-invalid";
      action = "drop";
      chain = "forward";
      comment = "defconf: drop invalid";
      connection_state = "invalid";
    }
    {
      name = "forward-drop-bad-src";
      action = "drop";
      chain = "forward";
      comment = "defconf: drop packets with bad src ipv6";
      src_address_list = "bad_ipv6";
    }
    {
      name = "forward-drop-bad-dst";
      action = "drop";
      chain = "forward";
      comment = "defconf: drop packets with bad dst ipv6";
      dst_address_list = "bad_ipv6";
    }
    {
      name = "forward-drop-hop-limit";
      action = "drop";
      chain = "forward";
      comment = "defconf: rfc4890 drop hop-limit=1";
      hop_limit = "equal:1";
      protocol = "icmpv6";
    }
    {
      name = "forward-accept-icmpv6";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept ICMPv6";
      protocol = "icmpv6";
    }
    {
      name = "forward-accept-hip";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept HIP";
      protocol = "139";
    }
    {
      name = "forward-accept-ike";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept IKE";
      dst_port = "500,4500";
      protocol = "udp";
    }
    {
      name = "forward-accept-ipsec-ah";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept ipsec AH";
      protocol = "ipsec-ah";
    }
    {
      name = "forward-accept-ipsec-esp";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept ipsec ESP";
      protocol = "ipsec-esp";
    }
    {
      name = "forward-accept-ipsec-policy";
      action = "accept";
      chain = "forward";
      comment = "defconf: accept all that matches ipsec policy";
      ipsec_policy = "in,ipsec";
    }
    {
      name = "forward-drop-not-lan";
      action = "drop";
      chain = "forward";
      comment = "defconf: drop everything else not coming from LAN";
      in_interface_list = "!LAN";
    }
  ];
in
{
  resource = {
    routeros_ip_firewall_filter = toResources filter;
    routeros_ip_firewall_nat = toResources nat;
    routeros_ipv6_firewall_filter = toResources filter6;

    routeros_ipv6_firewall_addr_list = lib.mapAttrs (_: entry: entry // { list = "bad_ipv6"; }) badIpv6;

    routeros_move_items = {
      filter = ordered "routeros_ip_firewall_filter" "/ip/firewall/filter" filter;
      nat = ordered "routeros_ip_firewall_nat" "/ip/firewall/nat" nat;
      filter6 = ordered "routeros_ipv6_firewall_filter" "/ipv6/firewall/filter" filter6;
    };
  };
}
