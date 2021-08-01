{ pkgs, ... }:

{
  services.jellyfin = { enable = true; };

  # networking.firewall.allowedTCPPorts = [ 80 443 ];

  # services.nginx = {
  #   enable = true;
  #   recommendedGzipSettings = true;
  #   recommendedOptimisation = true;
  #   recommendedProxySettings = true;
  #   recommendedTlsSettings = true;

  #   virtualHosts."home.paholg.com" = {
  #     addSSL = true;
  #     enableACME = true;
  #     locations."/" = {
  #       proxyPass = "http://localhost:8096";
  #     };
  #   };
  # };

  # security.acme = {
  #   acceptTerms = true;
  #   email = "paho@paholg.com";
  # };
}
