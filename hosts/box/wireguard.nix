{pkgs, ...}: let
  port = 51820;
in {
  networking = {
    nat = {
      enable = true;
      externalInterface = "eth0";
      internalInterfaces = ["wg0"];
    };
    firewall.allowedUDPPorts = [port];
    wireguard.interfaces.wg0 = {
      ips = ["10.100.0.1/24"];
      listenPort = port;

      # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
      # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
      postSetup = ''
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eno1 -j MASQUERADE
      '';

      # This undoes the above command
      postShutdown = ''
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eno1 -j MASQUERADE
      '';

      privateKeyFile = /home/paho/secrets/wireguard.key;

      peers = [
        {
          # Ang
          publicKey = "";
          allowedIPs = ["0.0.0.0/0" "::/0"];
        }
        {
          # ubuntu
          publicKey = "fGW4mrhH1lb63KiITNSpkJMZy6NypbRjDYN/X9xn9gE=";
          allowedIPs = ["0.0.0.0/0" "::/0"];
        }
      ];
    };
  };
}
