{ config, pkgs, ... }:
let
  foundryCommitHash = "f265e09";
  foundryVttSrc = pkgs.fetchFromGitHub {
    owner = "paholg";
    repo = "foundryvtt-docker";
    rev = foundryCommitHash;
    hash = "sha256-DsQrKIL0yLOHKnNizhioOp37v/P51RFJh7bl6O9+d6c=";
  };
  foundryVttImage = "localhost/foundryvtt:${foundryCommitHash}";
in
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

  # Build the foundry image from fork
  systemd.services.foundry-image-build = {
    description = "Build Foundry VTT Docker image from source";
    wantedBy = [ "multi-user.target" ];
    before = [ "podman-foundry.service" ];
    requiredBy = [ "podman-foundry.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    path = [ pkgs.podman ];
    script = ''
      podman build -t ${foundryVttImage} ${foundryVttSrc}
    '';
  };

  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.foundry = {
    user = "${toString config.custom.uids.foundry}:${toString config.custom.groups.foundry}";
    image = foundryVttImage;
    hostname = "vtt.paholg.com";
    ports = [ "0.0.0.0:${toString config.custom.ports.foundry}:30000" ];
    environmentFiles = [ config.age.secrets.foundry_env.path ];
    environment = {
      FOUNDRY_HOSTNAME = "vtt.paholg.com";
      FOUNDRY_PROXY_SSL = "true";
      FOUNDRY_PROXY_PORT = "443";
      FOUNDRY_TELEMETRY = "true";

      # Header-based authentication via oauth2-proxy
      CONTAINER_PATCH_URLS = "https://raw.githubusercontent.com/paholg/foundry-vtt-header-auth/refs/heads/autoprovision/patches.sh";
      AUTO_PROVISION = "true";
      ROLE_PLAYER = "foundry_vtt_player@auth.paholg.com";
      ROLE_ADMIN = "foundry_vtt_admin@auth.paholg.com";
    };
    volumes = [
      "${config.custom.drives.data}/foundry:/data"
    ];
  };
}
