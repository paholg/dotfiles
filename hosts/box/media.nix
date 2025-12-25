{
  config,
  lib,
  pkgs,
  ...
}:
let
  oauth2Port = toString config.custom.ports.oauth2_proxy;

  authExtraConfig = internalAuthLocation: ''
    auth_request ${internalAuthLocation};
    error_page 401 = /oauth2/sign_in;

    auth_request_set $user $upstream_http_x_auth_request_user;
    auth_request_set $preferred_username $upstream_http_x_auth_request_preferred_username;
    auth_request_set $email $upstream_http_x_auth_request_email;
    auth_request_set $groups $upstream_http_x_auth_request_groups;
    proxy_set_header X-User $user;
    proxy_set_header X-Email $email;

    auth_request_set $auth_cookie $upstream_http_set_cookie;
    add_header Set-Cookie $auth_cookie;
  '';

  mkAuthLocation =
    location: proxyPass: groups:
    let
      groupsParam = if groups == [ ] then "" else "?allowed_groups=${lib.concatStringsSep "," groups}";
      internalAuthLocation = "/internal/auth${location}";
    in
    {
      ${internalAuthLocation} = {
        extraConfig = ''
          internal;
          proxy_pass http://127.0.0.1:${oauth2Port}/oauth2/auth${groupsParam};
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Scheme $scheme;
          proxy_set_header Content-Length "";
          proxy_pass_request_body off;
        '';
      };
      ${location} = {
        inherit proxyPass;
        extraConfig = authExtraConfig internalAuthLocation;
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
              proxyPass = "http://127.0.0.1:${oauth2Port}";
              extraConfig = ''
                proxy_set_header X-Scheme $scheme;
                proxy_set_header X-Auth-Request-Redirect $request_uri;
              '';
            };
            "/internal/auth/" = {
              extraConfig = ''
                internal;
                proxy_pass http://127.0.0.1:${oauth2Port}/oauth2/auth;
                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Scheme $scheme;
                proxy_set_header Content-Length "";
                proxy_pass_request_body off;
              '';
            };
            "/" = {
              index = "index.html";
              extraConfig = ''
                ssi on;
                ${authExtraConfig "/internal/auth/"}
              '';
            };
          }
          (mkAuthLocation "/prowlarr" "http://localhost:${toString config.custom.ports.prowlarr}/prowlarr" [
            "arr_admin@auth.paholg.com"
          ])
          (mkAuthLocation "/radarr" "http://localhost:${toString config.custom.ports.radarr}/radarr" [
            "arr_admin@auth.paholg.com"
          ])
          (mkAuthLocation "/sonarr" "http://localhost:${toString config.custom.ports.sonarr}/sonarr" [
            "arr_admin@auth.paholg.com"
          ])
          (mkAuthLocation "/transmission"
            "http://${config.custom.ips.container}:${toString config.custom.ports.transmission}"
            [ "arr_admin@auth.paholg.com" ]
          )
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
        scope = "openid email profile groups";
        whitelist-domain = ".paholg.com";
      };
    };
  };

  systemd.services.jellyfin = {
    # Override UMask so that the `media` group gets file permissions
    serviceConfig.UMask = lib.mkForce "0002";
  };
}
