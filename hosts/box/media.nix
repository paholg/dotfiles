{
  config,
  lib,
  pkgs,
  ...
}:
{
  age.secrets = {
    porkbun_api = {
      file = ../../secrets/porkbun_api.json;
    };
    oauth2_proxy = {
      file = ../../secrets/oauth2_proxy;
    };
    tmdb_api_key = {
      file = ../../secrets/tmdb_api_key;
    };
  };

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  users.groups.media = {
    gid = config.custom.groups.media;
    members = [
      "paho"
      "nginx"
    ];
  };

  users.users.jellyfin.extraGroups = [
    "video"
    "render"
  ];
  users.users.plex.extraGroups = [
    "video"
    "render"
  ];

  environment.systemPackages = with pkgs; [ recyclarr ];

  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_18;
      dataDir = config.custom.drives.data + "/postgres";
      ensureDatabases = [ "bitmagnet" ];
      ensureUsers = [
        {
          name = "bitmagnet";
          ensureDBOwnership = true;
        }
      ];
      authentication = ''
        local bitmagnet bitmagnet peer
        host bitmagnet bitmagnet 127.0.0.1/32 trust
        host bitmagnet bitmagnet ::1/128 trust
      '';
    };

    jellyfin = {
      enable = true;
      group = "media";
      dataDir = config.custom.drives.data + "/jellyfin";
    };

    plex = {
      enable = true;
      group = "media";
      openFirewall = true; # Allows direct connection.
      dataDir = config.custom.drives.data + "/plex";
    };

    flaresolverr = {
      enable = true;
      port = config.custom.ports.flaresolverr;
    };

    prowlarr = {
      enable = true;
    };

    radarr = {
      enable = true;
      group = "media";
      dataDir = config.custom.drives.data + "/radarr";
    };

    sonarr = {
      enable = true;
      group = "media";
      dataDir = config.custom.drives.data + "/sonarr";
    };

    oauth2-proxy = {
      enable = true;
      provider = "oidc";
      httpAddress = "http://127.0.0.1:${toString config.custom.ports.oauth2_proxy}";
      reverseProxy = true;

      redirectURL = "https://home.paholg.com/oauth2/callback";
      oidcIssuerUrl = "https://auth.paholg.com/oauth2/openid/oauth2-proxy";

      keyFile = config.age.secrets.oauth2_proxy.path;

      email.domains = [ "*" ];

      setXauthrequest = true;

      cookie = {
        domain = ".paholg.com";
        secure = true;
      };

      extraConfig = {
        code-challenge-method = "S256";
        scope = "openid email profile groups";
        whitelist-domain = ".paholg.com";
        skip-provider-button = true;
      };
    };
  };

  systemd.services.jellyfin = {
    # Override UMask so that the `media` group gets file permissions
    serviceConfig.UMask = lib.mkForce "0002";
  };
}
