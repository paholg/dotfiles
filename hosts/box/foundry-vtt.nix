{
  config,
  pkgs,
  ...
}:
let
  version = "13";
  build = "351";
  fullVersion = "${version}.${build}";
  headerAuthRelease = "v13.351.0";

  foundryVttImage = "felddy/foundryvtt:${fullVersion}";
  foundryDownloadScript = pkgs.writeShellApplication {
    name = "foundry-download";
    runtimeInputs = [
      pkgs.xh
      pkgs.jq
      pkgs.ripgrep
    ];
    text = ''
      CACHE_DIR="/mnt/data/foundry/container_cache"
      VERSION="${fullVersion}"
      BUILD="${build}"
      DEST="$CACHE_DIR/foundryvtt-$VERSION.zip"

      if [[ -f "$DEST" ]]; then
        echo "foundryvtt-$VERSION.zip already cached, nothing to do."
        exit 0
      fi

      # shellcheck source=/dev/null
      source /run/agenix/foundry_env

      SESSION=$(mktemp -u)
      trap 'rm -f "$SESSION"' EXIT

      csrf=$(xh --check-status --session "$SESSION" GET https://foundryvtt.com/ | \
        rg -om1 'name="csrfmiddlewaretoken" value="([^"]+)"' -r '$1')

      xh --check-status --follow --session "$SESSION" --form --quiet POST \
        https://foundryvtt.com/auth/login/ \
        Referer:https://foundryvtt.com/ \
        csrfmiddlewaretoken="''${csrf}" \
        username="''${FOUNDRY_USERNAME}" \
        password="''${FOUNDRY_PASSWORD}" \
        next=/

      release_url=$(xh --check-status --session "$SESSION" GET \
        https://foundryvtt.com/releases/download \
        Referer:https://foundryvtt.com \
        build=="''${BUILD}" platform==node response_type==json | \
        jq -re .url)

      echo "Downloading foundryvtt-$VERSION.zip..."
      xh --check-status --follow --output "$DEST.tmp" GET "$release_url"
      mv "$DEST.tmp" "$DEST"
      chown foundry:foundry "$DEST"
      echo "Done."
    '';
  };
in
{
  age.secrets.foundry_env.file = ../../secrets/foundry_env;
  age.secrets.foundry_docker_env.file = ../../secrets/foundry_docker_env;

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
    image = foundryVttImage;
    hostname = "vtt.paholg.com";
    ports = [ "0.0.0.0:${toString config.custom.ports.foundry}:30000" ];
    environmentFiles = [ config.age.secrets.foundry_docker_env.path ];
    environment = {
      FOUNDRY_HOSTNAME = "vtt.paholg.com";
      FOUNDRY_PROXY_SSL = "true";
      FOUNDRY_PROXY_PORT = "443";
      FOUNDRY_TELEMETRY = "true";

      # Header-based authentication via oauth2-proxy
      CONTAINER_PATCH_URLS = "https://raw.githubusercontent.com/MaienM/foundry-vtt-header-auth/${headerAuthRelease}/patches.sh";
      AUTO_PROVISION = "true";
      ROLE_PLAYER = "foundry_vtt_player@auth.paholg.com";
      ROLE_ADMIN = "foundry_vtt_admin@auth.paholg.com";
    };
    volumes = [
      "${config.custom.drives.data}/foundry:/data"
    ];
  };

  environment.systemPackages = [ foundryDownloadScript ];

  system.activationScripts.foundryDownload = {
    text = "${foundryDownloadScript}/bin/foundry-download";
    deps = [ "agenix" ];
  };
}
