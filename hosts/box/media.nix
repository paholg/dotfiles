{
  config,
  lib,
  pkgs,
  ...
}:
let
  authExtraConfig = ''
    auth_request /oauth2/auth;
    error_page 401 = /oauth2/sign_in;

    auth_request_set $user $upstream_http_x_auth_request_user;
    auth_request_set $email $upstream_http_x_auth_request_email;
    proxy_set_header X-User $user;
    proxy_set_header X-Email $email;

    auth_request_set $auth_cookie $upstream_http_set_cookie;
    add_header Set-Cookie $auth_cookie;
  '';

  mkAuthLocation = location: proxyPass: {
    ${location} = {
      inherit proxyPass;
      extraConfig = authExtraConfig;
    };
  };
in
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

  security.acme = {
    acceptTerms = true;
    defaults.email = "paho@paholg.com";
  };

  environment.systemPackages = with pkgs; [ recyclarr ];

  services = {
    nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;

      virtualHosts."tv.paholg.com" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:${toString config.custom.ports.jellyfin}";
      };

      virtualHosts."home.paholg.com" = {
        enableACME = true;
        forceSSL = true;
        root = pkgs.runCommand "home-paholg-com-root" { } ''
          mkdir -p $out
          cp ${./home-index.html} $out/index.html
        '';

        locations = lib.mkMerge [
          {
            "/oauth2/" = {
              proxyPass = "http://127.0.0.1:${toString config.custom.ports.oauth2_proxy}";
              extraConfig = ''
                proxy_set_header X-Scheme $scheme;
                proxy_set_header X-Auth-Request-Redirect $request_uri;
              '';
            };
            "/" = {
              index = "index.html";
              extraConfig = authExtraConfig;
            };
          }
          (mkAuthLocation "/prowlarr" "http://localhost:${toString config.custom.ports.prowlarr}/prowlarr")
          (mkAuthLocation "/radarr" "http://localhost:${toString config.custom.ports.radarr}/radarr")
          (mkAuthLocation "/sonarr" "http://localhost:${toString config.custom.ports.sonarr}/sonarr")
          (mkAuthLocation "/transmission" "http://${config.custom.ips.container}:${toString config.custom.ports.transmission}")
        ];
      };
    };

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
      };
    };
  };

  systemd.services.jellyfin = {
    # Override UMask so that the `media` group gets file permissions
    serviceConfig.UMask = lib.mkForce "0002";
  };
}
