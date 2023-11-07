{...}: let
  hostIp = "10.233.1.1";
  containerIp = "10.233.1.2";
  transmissionPort = 9091;
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
    };

    config = {lib, ...}: {
      system.stateVersion = "20.03";

      users.groups.media = {gid = 1100;};

      networking.firewall.allowedTCPPorts = [transmissionPort];

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

      # Override for this issue:
      # https://github.com/NixOs/issues/258793
      systemd.services.transmission.serviceConfig = {
        RootDirectoryStartOnly = lib.mkForce false;
        RootDirectory = lib.mkForce "";
      };
    };
  };
}
