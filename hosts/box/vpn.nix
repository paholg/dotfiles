{...}: {
  networking.nat = {
    enable = true;
    externalInterface = "eno1";
    internalInterfaces = ["ve-+"];
    forwardPorts = [
      {
        # transmission's port
        destination = "10.233.1.2:9091";
        proto = "tcp";
        sourcePort = 9091;
      }
    ];
  };

  # Can't allow NetworkManager to manage container interfaces.
  networking.networkmanager.unmanaged = ["interface-name:ve-*"];

  containers.vpn = {
    autoStart = true;
    enableTun = true;
    privateNetwork = true;
    hostAddress = "10.233.1.1";
    localAddress = "10.233.1.2";

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

    config = {...}: {
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
        };
      };
    };
  };
}
