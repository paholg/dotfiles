{...}: let
  hostIp = "10.233.1.1";
  containerIp = "10.233.1.2";
  transmissionPort = 9091;
  media_gid = 1100;
in {
  networking.nat = {
    enable = true;
    externalInterface = "eno1";
    internalInterfaces = ["ve-+"];
  };

  # Can't allow NetworkManager to manage container interfaces.
  networking.networkmanager.unmanaged = ["interface-name:ve-*"];

  networking.firewall.allowedTCPPorts = [transmissionPort];

  containers.vpn = {
    autoStart = true;
    enableTun = true;
    privateNetwork = true;
    hostAddress = hostIp;
    localAddress = containerIp;

    bindMounts = {
      # Note: The container paths must match the host, as paths are provided
      # from transmission to Sonarr, etc.
      "/mnt/storage/downloads" = {
        hostPath = "/mnt/storage/downloads";
        isReadOnly = false;
      };
      "/mnt/storage/completed" = {
        hostPath = "/mnt/storage/completed";
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

    config = {lib, ...}: {
      system.stateVersion = "20.03";

      users.groups.media = {gid = media_gid;};

      networking.firewall.allowedTCPPorts = [transmissionPort];

      networking.nftables = {
        enable = true;
        tables.media_block = {
          enable = true;
          family = "inet";
          content = ''
            chain output {
              type filter hook output priority -100;

              oifname "tun0" accept
              ip daddr ${hostIp} accept

              skgid ${toString media_gid} drop
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
        group = "media";
        openFirewall = true;
        settings = {
          download-dir = "/mnt/storage/completed";
          incomplete-dir = "/mnt/storage/downloads";
          incomplete-dir-enabled = true;
          rpc-bind-address = "0.0.0.0";
          rpc-whitelist-enabled = false;
          download-queue-enabled = false;
        };
      };

      # TODO: Override for this issue:
      # https://github.com/NixOs/nixpkgs/issues/258793
      systemd.services.transmission.serviceConfig = {
        RootDirectoryStartOnly = lib.mkForce false;
        RootDirectory = lib.mkForce "";
      };
    };
  };
}
