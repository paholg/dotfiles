{
  config,
  pkgs,
  ...
}:
let
  downloads = config.custom.drives.storage + "/downloads";
  completed = config.custom.drives.storage + "/completed";
  transmission = config.custom.drives.storage + "/transmission";

  wgIP = "10.185.49.0";
  wgIP6 = "fd7d:76ee:e68f:a993:64ab:d868:53d:f267";

  # WireGuard peer configuration
  wgPeerPublicKey = "PyLCXAQT8KkM4T+dUsOQfn+Ub3pGxfGlxkIApuig+hk=";
  wgEndpointHost = "america3.vpn.airdns.org";
  wgEndpointPort = "1637";

  namespace = "vpn";
  vethHostIP = "10.200.1.1";
  vethVpnIP = config.custom.ips.vpn_veth;
in
{
  age.secrets = {
    wireguard_private_key.file = ../../secrets/wireguard_private_key;
    wireguard_preshared_key.file = ../../secrets/wireguard_preshared_key;
  };

  # VPN DNS for services in namespace
  environment.etc."resolv-vpn.conf".text = ''
    nameserver 10.128.0.1
  '';

  networking.wireguard.interfaces.wg0 = {
    ips = [ "${wgIP}/32" ];
    privateKeyFile = config.age.secrets.wireguard_private_key.path;
    interfaceNamespace = namespace;
    mtu = 1320;

    preSetup = ''
      ip netns add ${namespace} 2>/dev/null || true

      # Create veth pair for host <-> namespace communication
      ip link add veth-host type veth peer name veth-vpn 2>/dev/null || true
      ip link set veth-vpn netns ${namespace}
      ip addr add ${vethHostIP}/24 dev veth-host 2>/dev/null || true
      ip link set veth-host up
    '';

    peers = [
      {
        publicKey = wgPeerPublicKey;
        presharedKeyFile = config.age.secrets.wireguard_preshared_key.path;
        endpoint = "${wgEndpointHost}:${wgEndpointPort}";
        allowedIPs = [
          "0.0.0.0/0"
          "::/0"
        ];
        persistentKeepalive = 15;
      }
    ];

    postSetup = ''
      ip -n ${namespace} link set lo up
      ip -n ${namespace} addr add ${vethVpnIP}/24 dev veth-vpn
      ip -n ${namespace} link set veth-vpn up
      ip -n ${namespace} -6 addr add ${wgIP6}/128 dev wg0
      ip -n ${namespace} route add default dev wg0
      ip -n ${namespace} -6 route add default dev wg0
    '';

    postShutdown = ''
      ip link del veth-host 2>/dev/null || true
      ip netns del ${namespace} 2>/dev/null || true
    '';
  };

  # ****************************************************************************
  # Firewall and Port Forwarding
  # ****************************************************************************

  networking.firewall = {
    allowedTCPPorts = [ config.custom.ports.transmission_peer ];
    allowedUDPPorts = [ config.custom.ports.transmission_peer ];
    trustedInterfaces = [ "veth-host" ];
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
        host = "localhost";
        name = "bitmagnet";
        user = "bitmagnet";
        sslmode = "disable";
      };
    };
  };

  systemd.services.bitmagnet = {
    after = [
      "wireguard-wg0.service"
      "postgresql.service"
    ];
    requires = [
      "wireguard-wg0.service"
      "postgresql.service"
    ];
    serviceConfig = {
      NetworkNamespacePath = "/var/run/netns/${namespace}";
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
    after = [ "wireguard-wg0.service" ];
    requires = [ "wireguard-wg0.service" ];
    serviceConfig = {
      NetworkNamespacePath = "/var/run/netns/${namespace}";
      BindReadOnlyPaths = [ "/etc/resolv-vpn.conf:/etc/resolv.conf" ];
    };
  };

  # ****************************************************************************
  # PostgreSQL in Namespace
  # ****************************************************************************

  systemd.services.postgresql = {
    after = [ "wireguard-wg0.service" ];
    requires = [ "wireguard-wg0.service" ];
    serviceConfig = {
      NetworkNamespacePath = "/var/run/netns/${namespace}";
    };
  };
}
