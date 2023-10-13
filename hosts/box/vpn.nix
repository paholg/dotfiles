{...}: let
  hostIp = "10.233.1.1";
  containerIp = "10.233.1.2";
  transmissionPort = 9091;
in {
  networking.nat = {
    enable = true;
    externalInterface = "eno1";
    internalInterfaces = ["ve-+"];
    forwardPorts = [
      {
        destination = "${containerIp}:${builtins.toString transmissionPort}";
        proto = "tcp";
        sourcePort = transmissionPort;
      }
    ];
    # extraCommands = ''iptables -t nat -A nixos-nat-post -p tcp -d ${containerIp} --dport ${builtins.toString transmissionPort} -j SNAT --to-source ${hostIp}'';
  };

  # Can't allow NetworkManager to manage container interfaces.
  networking.networkmanager.unmanaged = ["interface-name:ve-*"];

  networking.firewall.allowedTCPPorts = [9091];

  containers.vpn = {
    autoStart = true;
    enableTun = true;
    privateNetwork = true;
    hostAddress = hostIp;
    localAddress = containerIp;
    forwardPorts = [
      {
        protocol = "tcp";
        hostPort = transmissionPort;
        containerPort = transmissionPort;
      }
    ];

    bindMounts = {
      "/downloads" = {
        hostPath = "/mnt/storage/downloads";
        isReadOnly = false;
      };
      "/completed" = {
        hostPath = "/mnt/storage/completed";
        isReadOnly = false;
      };
      "/ca_vancouver.ovpn" = {
        hostPath = "/home/paho/dotfiles/hosts/box/ca_vancouver.ovpn";
        isReadOnly = true;
      };
    };

    config = {lib, ...}: {
      users.groups.media = {};

      services.openvpn.servers.pia = {
        authUserPass = {
          # NOTE: These end up world-readable. Not secure!
          username = builtins.readFile ./PIA_USERNAME;
          password = builtins.readFile ./PIA_PASSWORD;
        };
        config = "config /ca_vancouver.ovpn";
      };

      services.transmission = {
        enable = true;
        group = "media";
        openFirewall = true;
        settings = {
          download-dir = "/completed";
          incomplete-dir = "/downloads";
          incomplete-dir-enabled = true;
          rpc-bind-address = "0.0.0.0";
          rpc-whitelist-enabled = false;
        };
      };

      # Override for this issue:
      # https://github.com/NixOs/issues/258793
      systemd.services.transmission.serviceConfig = {
        RootDirectoryStartOnly = lib.mkForce false;
        RootDirectory = lib.mkForce "";
      };
    };
  };
}
