{ config, pkgs, ... }:
let
  cfg = config.custom;

  downloads = cfg.drives.storage + "/downloads";
  completed = cfg.drives.storage + "/completed";
  transmission = cfg.drives.storage + "/transmission";

  ca_vancouver = (pkgs.writeText "ca_vancouver.ovpn" (builtins.readFile ./ca_vancouver.ovpn)).outPath;
in
{
  config = {
    networking.nat = {
      enable = true;
      externalInterface = "eno1";
      internalInterfaces = [ "ve-+" ];
    };

    # Can't allow NetworkManager to manage container interfaces.
    networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];

    networking.firewall.allowedTCPPorts = [ cfg.ports.transmission ];

    containers.vpn = {
      ephemeral = true;
      autoStart = true;
      enableTun = true;
      privateNetwork = true;
      hostAddress = cfg.ips.host;
      localAddress = cfg.ips.container;

      bindMounts = {
        # Note: The container paths must match the host, as paths are provided
        # from transmission to Sonarr, etc.
        "${downloads}".isReadOnly = false;
        "${completed}".isReadOnly = false;
        "${transmission}".isReadOnly = false;
        "/ca_vancouver.ovpn" = {
          hostPath = ca_vancouver;
          isReadOnly = true;
        };
        "${config.age.secrets.pia.path}".isReadOnly = true;
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

          services.openvpn.servers.pia = {
            config = ''
              config /ca_vancouver.ovpn
              auth-user-pass ${config.age.secrets.pia.path}
            '';
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
              # NOTE: This mask needs to be specified in base 10 instead of octal.
              umask = 7; # 0o007 == 7
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
