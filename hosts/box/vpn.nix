{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.custom.vpn;

  downloads = cfg.storage + "/downloads";
  completed = cfg.storage + "/completed";
  transmission = cfg.storage + "/transmission";

  group = "media";
in {
  options.custom.vpn = {
    enable = mkEnableOption "Vpn runnint with torrents";

    ips = mkOption {type = types.attrsOf types.str;};

    media_gid = mkOption {type = types.int;};

    transmission_port = mkOption {type = types.int;};

    storage = mkOption {type = types.str;};
  };

  config = mkIf cfg.enable {
    networking.nat = {
      enable = true;
      externalInterface = "eno1";
      internalInterfaces = ["ve-+"];
    };

    # Can't allow NetworkManager to manage container interfaces.
    networking.networkmanager.unmanaged = ["interface-name:ve-*"];

    networking.firewall.allowedTCPPorts = [cfg.transmission_port];

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
        "${downloads}" = {
          hostPath = downloads;
          isReadOnly = false;
        };
        "${completed}" = {
          hostPath = completed;
          isReadOnly = false;
        };
        "${transmission}" = {
          hostPath = transmission;
          isReadOnly = false;
        };
        "/ca_vancouver.ovpn" = {
          hostPath = "/home/paho/dotfiles/hosts/box/ca_vancouver.ovpn";
          isReadOnly = true;
        };
        # Fixes issues with DNS in the container.
        "/etc/resolv.conf" = {
          hostPath = "/etc/resolv.conf";
          isReadOnly = true;
        };
      };

      config = {
        lib,
        pkgs,
        ...
      }: {
        system.stateVersion = "20.03";
        environment.systemPackages = with pkgs; [dig dnsutils];

        users.groups.media = {
          gid = cfg.media_gid;
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

                skgid ${toString cfg.media_gid} drop
              }
            '';
          };
        };

        services.openvpn.servers.pia = {
          authUserPass = {
            # NOTE: These end up world-readable. Not secure!
            username = builtins.readFile /home/paho/secrets/pia_username;
            password = builtins.readFile /home/paho/secrets/pia_password;
          };
          config = "config /ca_vancouver.ovpn";
        };

        services.transmission = {
          enable = true;
          inherit group;
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
        # FIXED now????
        # systemd.services.transmission.serviceConfig = {
        #   RootDirectoryStartOnly = lib.mkForce false;
        #   RootDirectory = lib.mkForce "";
        # };
      };
    };
  };
}
