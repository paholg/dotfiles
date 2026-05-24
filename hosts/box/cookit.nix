{ config, ... }:
{
  # users.groups.cookit = { };

  age.secrets."cookit_client_secret" = {
    file = ../../secrets/cookit_client_secret;
    owner = "authit";
  };

  age.secrets."cookit_session_secret" = {
    file = ../../secrets/cookit_session_secret;
    owner = "authit";
  };

  services.cookit = {
    enable = true;
    cookitUrl = "https://cookit.paholg.com";
    dataDir = "/mnt/data/cookit";
    ipAddress = "127.0.0.1";
    port = config.custom.ports.cookit;
    sessionSecretFile = config.age.secrets.cookit_session_secret.path;

    oidc = {
      issuerUrl = "https://auth.paholg.com/oauth2/openid/cookit";
      clientId = "cookit";
      clientSecretFile = config.age.secrets.cookit_client_secret.path;
      adminGroup = "cookit_admin@auth.paholg.com";
      userGroup = "cookit_user@auth.paholg.com";
    };
  };

  services.nginx.virtualHosts."cookit.paholg.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.custom.ports.cookit}";
    };
  };
}
