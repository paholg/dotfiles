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

  users.users.paho.extraGroups = [
    "foundry"
    "docker"
  ];

  virtualisation.oci-containers.backend = "podman";
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

      DEBUG = "*";

      # Header-based authentication via oauth2-proxy
      CONTAINER_PATCH_URLS = "https://github.com/MaienM/foundry-vtt-header-auth/releases/download/v13.348.0/patches.sh";
      ROLE_PLAYER = "foundry_vtt_player@auth.paholg.com";
      ROLE_ADMIN = "foundry_vtt_admin@auth.paholg.com";
    };
    volumes = [
      "${config.custom.drives.storage}/foundry:/data"
      # Keep the install until the cache is fixed:
      # https://github.com/felddy/foundryvtt-docker/issues/1333
      "${config.custom.drives.storage}/foundry/resources:/home/node/resources"
    ];
  };
}
