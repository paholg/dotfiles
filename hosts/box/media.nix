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
    vpn_config = {
      file = ../../secrets/vpn_config;
    };
    oauth2_proxy = {
      file = ../../secrets/oauth2_proxy;
    };
  };

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  users.groups.media = {
    gid = config.custom.groups.media;
    members = [ "paho" ];
  };

  environment.systemPackages = with pkgs; [ recyclarr ];

  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_16;
      dataDir = config.custom.drives.storage + "/postgres";
    };

    jellyfin = {
      enable = true;
      group = "media";
    };

    plex = {
      enable = true;
      group = "media";
      openFirewall = false;
      dataDir = config.custom.drives.storage + "/plex";
    };

    prowlarr = {
      enable = true;
      openFirewall = false;
    };

    radarr = {
      enable = true;
      group = "media";
      openFirewall = false;
      dataDir = config.custom.drives.storage + "/radarr";
    };

    sonarr = {
      enable = true;
      group = "media";
      openFirewall = false;
      dataDir = config.custom.drives.storage + "/sonarr";
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
