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
    ports = [ "0.0.0.0:${toString config.custom.ports.foundry}:30000" ];
    environmentFiles = [ config.age.secrets.foundry_env.path ];
    volumes = [ "${config.custom.drives.storage}/foundry:/data" ];
  };

  services.nginx.virtualHosts."vtt.paholg.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString config.custom.ports.foundry}";
      proxyWebsockets = true;
    };
  };
}
