{ config, ... }:
{
  users.groups.authit = { };

  age.secrets."authit_config.toml" = {
    file = ../../secrets/authit_config.toml;
    owner = "authit";
  };

  services.authit = {
    enable = true;
    kanidmUrl = "https://auth.paholg.com";
    authitUrl = "https://authit.paholg.com";
    adminGroup = "authit_admin@auth.paholg.com";
    oauthClientId = "authit";
    ipAddress = "127.0.0.1";
    port = config.custom.ports.authit;
    configFile = config.age.secrets."authit_config.toml".path;
  };

  systemd.services.authit.serviceConfig.SupplementaryGroups = [ "authit" ];

  services.nginx.virtualHosts."authit.paholg.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.custom.ports.authit}";
    };
  };
}
