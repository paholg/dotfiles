{ ... }:
let
  port = 51820;
in
{
  networking.firewall.allowedUDPPorts = [ port ];
  systemd.network = {
    enable = true;
    netdevs = {
      "50-wg0" = {
        netdevConfig = {
          Kind = "wireguard";
          Name = "wg0";
          MTUBytes = "1300";
        };
        wireguardConfig = {
          PrivateKeyFile = "/wireguard.key";
          ListenPort = port;
        };
        wireguardPeers = [
          # {
          #   # Ang - 10.100.0.2
          #   wireguardPeerConfig = {
          #     PublicKey = "";
          #     AllowedIPs = ["0.0.0.0/0" "::/0"];
          #   };
          # }
          {
            # Pixel 5 - 10.100.0.3
            wireguardPeerConfig = {
              PublicKey = "M19tsWObxoO35sBuMTUme71+RhoHx0MKphj0UDnArX8=";
              AllowedIPs = [
                "0.0.0.0/0"
                "::/0"
              ];
            };
          }
          {
            # Ubuntu - 10.100.0.4
            wireguardPeerConfig = {
              PublicKey = "fGW4mrhH1lb63KiITNSpkJMZy6NypbRjDYN/X9xn9gE=";
              AllowedIPs = [
                "0.0.0.0/0"
                "::/0"
              ];
            };
          }
        ];
      };
    };
    networks.wg0 = {
      matchConfig.Name = "wg0";
      address = [ "10.100.0.1/24" ];
      networkConfig = {
        IPMasquerade = "ipv4";
        IPForward = true;
      };
    };
  };
}
