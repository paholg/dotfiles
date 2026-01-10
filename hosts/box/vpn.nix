{
  config,
  pkgs,
  ...
}:
let
  downloads = config.custom.drives.storage + "/downloads";
  completed = config.custom.drives.storage + "/completed";
  transmission = config.custom.drives.storage + "/transmission";
  rtorrent = config.custom.drives.storage + "/rtorrent";

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
    allowedTCPPorts = [
      config.custom.ports.transmission_peer
      config.custom.ports.rtorrent_peer
    ];
    allowedUDPPorts = [
      config.custom.ports.transmission_peer
      config.custom.ports.rtorrent_peer
      config.custom.ports.rtorrent_dht
      config.custom.ports.bitmagnet_dht
    ];
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
      dht_server.port = config.custom.ports.bitmagnet_dht;
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
  # rTorrent Service
  # ****************************************************************************

  services.rtorrent = {
    enable = true;
    port = config.custom.ports.rtorrent_peer;
    group = "media";
    dataDir = rtorrent;
    downloadDir = downloads;
    openFirewall = true;
    configText = ''
      # See https://github.com/rakshasa/rtorrent/wiki/Performance-Tuning
      # TERMINOLOGY:
      # A "slot" is a peer that is transferring data.

      network.bind_address.set = 0.0.0.0

      # Global upload and download rate in KiB, `0` for unlimited (`download_rate`, `upload_rate`)
      throttle.global_down.max_rate.set_kb = 0
      throttle.global_up.max_rate.set_kb   = 350000

      # Maximum number of simultaneous downloads and uploads slots (global slots!) (`max_downloads_global`, `max_uploads_global`)
      throttle.max_downloads.global.set = 100
      throttle.max_uploads.global.set   = 100

      throttle.max_downloads.div.set = 100
      throttle.max_uploads.div.set   = 100
      network.http.max_open.set = 100

      # Maximum and minimum number of peers to connect to per torrent while downloading (`min_peers`, `max_peers`) Default: `100` and `200` respectively
      throttle.min_peers.normal.set = 49
      throttle.max_peers.normal.set = 50

      # Same as above but for seeding completed torrents (seeds per torrent), `-1` for same as downloading (`min_peers_seed`, `max_peers_seed`) Default: `-1` for both
      throttle.min_peers.seed.set = -1
      throttle.max_peers.seed.set = -1

      # Maximum number of simultaneous downloads and uploads slots per torrent (`max_uploads`) Default: `50` for both
      throttle.max_downloads.set = 50
      throttle.max_uploads.set = 50

      # Set the numwant field sent to the tracker, which indicates how many peers we want. 
      #  A negative value disables this feature. Default: `-1`
      trackers.numwant.set = 50

      # Set the max amount of memory address space used to mapping file chunks. This refers to memory mapping, not
      #  physical memory allocation. Default: `1GB` (`max_memory_usage`) 
      # This may also be set using ulimit -m where 3/4 will be allocated to file chunks.
      pieces.memory.max.set = 8192M

      # Maximum number of connections rtorrent can accept/make
      network.max_open_sockets.set = 5000

      # Maximum number of open files rtorrent can keep open (you have to modify the system wide settings with ulimit!)
      network.max_open_files.set = 10240

      # Maximum number of simultaneous HTTP request (used by announce or scrape requests) Default: `32`
      network.http.max_open.set = 99

      # Send and receive buffer size for socket. Disabled by default (`0`), this means the default is used by OS 
      #  (you have to modify the system wide settings!)
      # Increasing buffer sizes may help reduce disk seeking, connection polling as more data is buffered each time
      #  the socket is written to. It will result higher memory usage (not visible in rtorrent process!).
      network.receive_buffer.size.set =  4M
      network.send_buffer.size.set    = 12M

      # Preloading a piece of a file. Default: `0` Possible values: `0` (Off) , `1` (Madvise) , `2` (Direct paging).
      pieces.preload.type.set = 2
      #pieces.preload.min_size.set = 262144
      #pieces.preload.min_rate.set = 5120

      # TOS of peer connections. Default: `throughput`. If the option is set to `default` then the system default TOS
      #  is used. A hex value may be used for non-standard settings.
      # Possible values: `[default|lowdelay|throughput|reliability|mincost]` or a hex value.
      #network.tos.set = throughput

      # CURL options to add skip TLS verification (for nonofficial SSL trackers)
      network.http.ssl_verify_host.set = 0
      network.http.ssl_verify_peer.set = 0

      # CURL option to lower DNS timeout. Default: `60`.
      network.http.dns_cache_timeout.set = 25

      # Max packet size using xmlrpc. Default: `524288`
      network.xmlrpc.size_limit.set = 2M

      # Save all the sessions in every 12 hours instead of the default 20 minutes.
      schedule2 = session_save, 1200, 43200, ((session.save))

      # Prune file status in every 24 hours, this is the default setting.
      #schedule2 = prune_file_status, 3600, 86400, ((system.file_status_cache.prune))

      # Whether to allocate disk space for a new torrent. Default: `0`
      #system.file.allocate.set = 1

      dht.mode.set = auto
      dht.port.set = ${toString config.custom.ports.rtorrent_dht}
      protocol.pex.set = yes

      trackers.use_udp.set = yes
    '';
  };

  systemd.services.rtorrent = {
    after = [ "wireguard-wg0.service" ];
    requires = [ "wireguard-wg0.service" ];
    serviceConfig = {
      NetworkNamespacePath = "/var/run/netns/${namespace}";
      BindReadOnlyPaths = [ "/etc/resolv-vpn.conf:/etc/resolv.conf" ];
      UMask = "0002";
      LimitNOFILE = 100000;
      # Allow chown for socket permission setup (blocked by default @privileged filter)
      SystemCallFilter = [ "fchownat" ];
    };
  };

  services.flood = {
    enable = true;
    host = "0.0.0.0";
    port = config.custom.ports.rtorrent;
    extraArgs = [
      "--auth=none"
      "--baseuri=/rtorrent"
      "--rtsocket=${config.services.rtorrent.rpcSocket}"
    ];
  };

  systemd.services.flood = {
    after = [
      "wireguard-wg0.service"
      "rtorrent.service"
    ];
    requires = [
      "wireguard-wg0.service"
      "rtorrent.service"
    ];
    serviceConfig = {
      NetworkNamespacePath = "/var/run/netns/${namespace}";
      BindReadOnlyPaths = [ "/etc/resolv-vpn.conf:/etc/resolv.conf" ];
      SupplementaryGroups = [ config.services.rtorrent.group ];
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
