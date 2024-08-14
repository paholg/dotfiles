{ config, ... }:
let
  cfg = config.custom;

  downloads = cfg.drives.storage + "/downloads";
  completed = cfg.drives.storage + "/completed";
  transmission = cfg.drives.storage + "/transmission";

  ovpn = config.age.secrets.vpn_config.path;
  peer_port = 23014;
in
{
  config = {
    networking.nat = {
      enable = true;
      externalInterface = "eno2";
      internalInterfaces = [ "ve-+" ];
    };

    # Can't allow NetworkManager to manage container interfaces.
    networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];

    networking.firewall.allowedTCPPorts = [
      cfg.ports.transmission
      peer_port
    ];

    containers.vpn = {
      ephemeral = true;
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      hostAddress = cfg.ips.host;
      localAddress = cfg.ips.container;

      forwardPorts = [ { hostPort = peer_port; } ];

      bindMounts = {
        # Note: The container paths must match the host, as paths are provided
        # from transmission to Sonarr, etc.
        "${downloads}".isReadOnly = false;
        "${completed}".isReadOnly = false;
        "${transmission}".isReadOnly = false;
        "/config.ovpn" = {
          hostPath = ovpn;
          isReadOnly = true;
        };
        # Fix DNS
        "/etc/resolv.conf".isReadOnly = true;
      };

      config =
        { lib, pkgs, ... }:
        {
          system.stateVersion = "20.03";
          environment.systemPackages = with pkgs; [
            dig
            dnsutils
          ];

          users.groups.media = {
            gid = cfg.groups.media;
          };

          networking.nftables = {
            enable = true;
            tables.media_block = {
              enable = true;
              family = "inet";
              content = ''
                chain output {
                  type filter hook output priority -100;

                  oifname "tun0" accept
                  ip daddr ${cfg.ips.host} accept

                  skgid ${toString cfg.groups.media} drop
                }
              '';
            };
          };

          services.openvpn.servers.air = {
            config = "config /config.ovpn";
            updateResolvConf = true;
          };

          services.transmission = {
            enable = true;
            group = "media";
            openFirewall = true;
            home = transmission;
            webHome = pkgs.flood-for-transmission;
            openRPCPort = true;
            openPeerPorts = true;
            downloadDirPermissions = "770";
            settings = {
              download-dir = completed;
              incomplete-dir = downloads;
              incomplete-dir-enabled = true;
              rpc-bind-address = cfg.ips.container;
              rpc-whitelist-enabled = false;
              download-queue-enabled = false;
              seed-queue-enabled = false;
              # NOTE: This mask needs to be specified in base 10 instead of octal.
              umask = 7; # 0o007 == 7
              cache-size-mb = 1024;
              peer-limit-per-torrent = 250;
              peer-limit-global = 10000;
              peer-port = peer_port;
            };
          };

          # TODO: Override for this issue:
          # https://github.com/NixOs/nixpkgs/issues/258793
          # As of 2024-07-18, still not fixed, despite that issue being closed.
          systemd.services.transmission.serviceConfig = {
            RootDirectoryStartOnly = lib.mkForce false;
            RootDirectory = lib.mkForce "";
          };
        };
    };
  };
}
