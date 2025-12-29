{
  config,
  lib,
  pkgs,
  ...
}:
let
  downloads = config.custom.drives.storage + "/downloads";
  completed = config.custom.drives.storage + "/completed";
  transmission = config.custom.drives.storage + "/transmission";

  # Network namespace configuration
  vpnNamespace = "vpn";
  vethHostIP = "10.200.1.1";
  vethVpnIP = "10.200.1.2";
  vethSubnet = "10.200.1.0/24";
  wgIP = "10.185.49.0";
  wgIP6 = "fd7d:76ee:e68f:a993:64ab:d868:53d:f267";
  vpnDNS = "10.128.0.1";

  # WireGuard peer configuration (public info, not secret)
  wgPeerPublicKey = "PyLCXAQT8KkM4T+dUsOQfn+Ub3pGxfGlxkIApuig+hk=";
  wgEndpointHost = "america3.vpn.airdns.org";
  wgEndpointPort = "1637";
in
{
  age.secrets = {
    wireguard_private_key.file = ../../secrets/wireguard_private_key;
    wireguard_preshared_key.file = ../../secrets/wireguard_preshared_key;
  };

  # VPN DNS for services in namespace
  environment.etc."resolv-vpn.conf".text = ''
    nameserver ${vpnDNS}
  '';

  # ****************************************************************************
  # Network Namespace Setup
  # ****************************************************************************

  systemd.services.vpn-namespace = {
    description = "VPN network namespace setup";
    wantedBy = [ "multi-user.target" ];
    before = [ "wireguard-vpn.service" ];

    path = [ pkgs.iproute2 ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "vpn-ns-up" ''
        set -e

        # Create namespace
        ip netns add ${vpnNamespace}

        # Create veth pair for host<->namespace communication
        ip link add veth-host type veth peer name veth-vpn
        ip link set veth-vpn netns ${vpnNamespace}

        # Configure host side
        ip addr add ${vethHostIP}/24 dev veth-host
        ip link set veth-host up

        # Configure namespace side
        ip netns exec ${vpnNamespace} ip addr add ${vethVpnIP}/24 dev veth-vpn
        ip netns exec ${vpnNamespace} ip link set veth-vpn up
        ip netns exec ${vpnNamespace} ip link set lo up
      '';
      ExecStop = pkgs.writeShellScript "vpn-ns-down" ''
        ip netns del ${vpnNamespace} || true
        ip link del veth-host 2>/dev/null || true
      '';
    };
  };

  # ****************************************************************************
  # WireGuard in Namespace
  # ****************************************************************************

  systemd.services.wireguard-vpn = {
    description = "WireGuard VPN in namespace";
    after = [
      "vpn-namespace.service"
      "network-online.target"
    ];
    requires = [ "vpn-namespace.service" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    path = [
      pkgs.iproute2
      pkgs.wireguard-tools
    ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "wg-up" ''
        set -e

        # Clean up any existing wg0 interface from failed previous runs
        ip link del wg0 2>/dev/null || true
        ip netns exec ${vpnNamespace} ip link del wg0 2>/dev/null || true

        # Resolve endpoint hostname in root namespace (has DNS access)
        ENDPOINT_IP=$(${pkgs.dig}/bin/dig +short ${wgEndpointHost} | head -n1)
        if [ -z "$ENDPOINT_IP" ]; then
          echo "Failed to resolve ${wgEndpointHost}"
          exit 1
        fi
        echo "Resolved ${wgEndpointHost} to $ENDPOINT_IP"

        # Create WireGuard interface in root namespace, then move to vpn namespace
        ip link add wg0 type wireguard
        ip link set wg0 netns ${vpnNamespace}

        # Configure WireGuard using separate key files
        ip netns exec ${vpnNamespace} wg set wg0 \
          private-key ${config.age.secrets.wireguard_private_key.path}

        ip netns exec ${vpnNamespace} wg set wg0 \
          peer ${wgPeerPublicKey} \
          preshared-key ${config.age.secrets.wireguard_preshared_key.path} \
          endpoint $ENDPOINT_IP:${wgEndpointPort} \
          allowed-ips 0.0.0.0/0,::/0 \
          persistent-keepalive 15

        # Configure IP addresses
        ip netns exec ${vpnNamespace} ip addr add ${wgIP}/32 dev wg0
        ip netns exec ${vpnNamespace} ip -6 addr add ${wgIP6}/128 dev wg0
        ip netns exec ${vpnNamespace} ip link set wg0 mtu 1320
        ip netns exec ${vpnNamespace} ip link set wg0 up

        # Route all traffic through WireGuard (except veth subnet for host communication)
        ip netns exec ${vpnNamespace} ip route add default dev wg0
        ip netns exec ${vpnNamespace} ip -6 route add default dev wg0
      '';
      ExecStop = pkgs.writeShellScript "wg-down" ''
        ip netns exec ${vpnNamespace} ip link del wg0 2>/dev/null || true
      '';
    };
  };

  # ****************************************************************************
  # Firewall and Port Forwarding
  # ****************************************************************************

  networking.firewall = {
    allowedTCPPorts = [
      config.custom.ports.transmission_peer
    ];
    allowedUDPPorts = [
      config.custom.ports.transmission_peer
    ];

    # Trust the veth interface for host<->namespace communication
    trustedInterfaces = [ "veth-host" ];

    # Forward ports to namespace and enable NAT for namespace traffic
    extraCommands = ''
      # Forward transmission peer port to namespace
      iptables -t nat -A PREROUTING -p tcp --dport ${toString config.custom.ports.transmission_peer} -j DNAT --to-destination ${vethVpnIP}:${toString config.custom.ports.transmission_peer}
      iptables -t nat -A PREROUTING -p udp --dport ${toString config.custom.ports.transmission_peer} -j DNAT --to-destination ${vethVpnIP}:${toString config.custom.ports.transmission_peer}

      # Enable masquerading for namespace traffic (outbound and inbound)
      iptables -t nat -A POSTROUTING -s ${vethSubnet} -j MASQUERADE
      iptables -t nat -A POSTROUTING -d ${vethSubnet} -j MASQUERADE
    '';
    extraStopCommands = ''
      iptables -t nat -D PREROUTING -p tcp --dport ${toString config.custom.ports.transmission_peer} -j DNAT --to-destination ${vethVpnIP}:${toString config.custom.ports.transmission_peer} 2>/dev/null || true
      iptables -t nat -D PREROUTING -p udp --dport ${toString config.custom.ports.transmission_peer} -j DNAT --to-destination ${vethVpnIP}:${toString config.custom.ports.transmission_peer} 2>/dev/null || true
      iptables -t nat -D POSTROUTING -s ${vethSubnet} -j MASQUERADE 2>/dev/null || true
      iptables -t nat -D POSTROUTING -d ${vethSubnet} -j MASQUERADE 2>/dev/null || true
    '';
  };

  # ****************************************************************************
  # Bitmagnet Service
  # ****************************************************************************

  services.bitmagnet = {
    enable = true;
    group = "media";
    useLocalPostgresDB = false;
    settings = {
      http_server.port = ":${toString config.custom.ports.bitmagnet}";
      processor.concurrency = 4;
      postgres = {
        host = vethHostIP;
        name = "bitmagnet";
        user = "bitmagnet";
      };
    };
  };

  systemd.services.bitmagnet = {
    after = [
      "wireguard-vpn.service"
      "postgresql.service"
    ];
    requires = [ "wireguard-vpn.service" ];
    serviceConfig = {
      NetworkNamespacePath = "/var/run/netns/${vpnNamespace}";
      BindReadOnlyPaths = [ "/etc/resolv-vpn.conf:/etc/resolv.conf" ];
      EnvironmentFile = config.age.secrets.tmdb_api_key.path;
    };
  };

  # ****************************************************************************
  # Transmission Service
  # ****************************************************************************

  services.transmission = {
    enable = true;
    package = pkgs.transmission_4;
    group = "media";
    home = transmission;
    webHome = pkgs.flood-for-transmission;
    openRPCPort = true;
    openPeerPorts = true;
    downloadDirPermissions = "770";
    settings = {
      download-dir = completed;
      incomplete-dir = downloads;
      incomplete-dir-enabled = true;
      rpc-bind-address = "0.0.0.0";
      rpc-host-whitelist-enabled = true;
      rpc-host-whitelist = "home.paholg.com";
      rpc-whitelist-enabled = false;
      download-queue-enabled = false;
      seed-queue-enabled = true;
      ratio-limit = 2.0;
      # NOTE: This mask needs to be specified in base 10 instead of octal.
      umask = 7; # 0o007 == 7
      cache-size-mb = 1024;
      peer-limit-per-torrent = 250;
      peer-limit-global = 10000;
      peer-port = config.custom.ports.transmission_peer;
    };
  };

  systemd.services.transmission = {
    after = [ "wireguard-vpn.service" ];
    requires = [ "wireguard-vpn.service" ];
    serviceConfig = {
      NetworkNamespacePath = "/var/run/netns/${vpnNamespace}";
      BindReadOnlyPaths = [ "/etc/resolv-vpn.conf:/etc/resolv.conf" ];
    };
  };

  # ****************************************************************************
  # PostgreSQL Access from Namespace
  # ****************************************************************************

  services.postgresql = {
    settings.listen_addresses = lib.mkForce "localhost,${vethHostIP}";
    authentication = lib.mkAfter ''
      host bitmagnet bitmagnet ${vethSubnet} trust
    '';
  };
}
