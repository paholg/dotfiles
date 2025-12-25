{ config, ... }:
{
  age.secrets.foundry_env.file = ../../secrets/foundry_env;

  users.groups.foundry = {
    gid = config.custom.groups.foundry;
  };

  users.users.foundry = {
    isSystemUser = true;
    group = "foundry";
    uid = config.custom.uids.foundry;
  };

  users.users.paho.extraGroups = [ "foundry" ];

  virtualisation.oci-containers.containers.foundry = {
    user = "${toString config.custom.uids.foundry}:${toString config.custom.groups.foundry}";
    image = "felddy/foundryvtt:13.351";
    hostname = "vtt.paholg.com";
    ports = [ "0.0.0.0:${toString config.custom.ports.foundry}:30000" ];
    environmentFiles = [ config.age.secrets.foundry_env.path ];
    environment = {
      FOUNDRY_HOSTNAME = "vtt.paholg.com";
      FOUNDRY_PROXY_SSL = "true";
      FOUNDRY_PROXY_PORT = "443";
    };
    volumes = [
      "${config.custom.drives.storage}/foundry:/data"
      # Keep the install until the cache is fixed:
      # https://github.com/felddy/foundryvtt-docker/issues/1333
      "${config.custom.drives.storage}/foundry/resources:/home/node/resources"
    ];
  };

  services.nginx.virtualHosts."vtt.paholg.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString config.custom.ports.foundry}";
      proxyWebsockets = true;
      extraConfig = ''
        # Disable proxy buffering for faster streaming
        proxy_buffering off;

        # Increase max body size for asset uploads
        client_max_body_size 300M;
      '';
    };
  };
}
