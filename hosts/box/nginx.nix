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
    {
      location,
      proxyPass,
      groups ? [ ],
      extraConfig ? "",
      signInUrl ? "/oauth2/sign_in",
      headers ? ''
        proxy_set_header X-User $user;
        proxy_set_header X-Email $email;
      '',
    }:
    let
      groupsParam = if groups == [ ] then "" else "?allowed_groups=${lib.concatStringsSep "," groups}";
      internalAuthLocation = "/internal/auth${location}";
      isCrossDomain = lib.hasPrefix "http" signInUrl;
      errorDirective = if isCrossDomain then "@error401${location}" else signInUrl;
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

          # oauth2-proxy sends large headers (JWT tokens)
          proxy_buffer_size 128k;
          proxy_buffers 4 256k;
          proxy_busy_buffers_size 256k;
        '';
      };
      ${location} = {
        inherit proxyPass;
        proxyWebsockets = true;
        extraConfig = ''
          auth_request ${internalAuthLocation};
          error_page 401 = ${errorDirective};

          auth_request_set $user $upstream_http_x_auth_request_user;
          auth_request_set $preferred_username $upstream_http_x_auth_request_preferred_username;
          auth_request_set $email $upstream_http_x_auth_request_email;
          auth_request_set $groups $upstream_http_x_auth_request_groups;
          ${headers}

          auth_request_set $auth_cookie $upstream_http_set_cookie;
          add_header Set-Cookie $auth_cookie;

          ${extraConfig}
        '';
      };
    }
    // lib.optionalAttrs isCrossDomain {
      "${errorDirective}" = {
        extraConfig = ''
          return 302 ${signInUrl}?rd=$scheme://$host$request_uri;
        '';
      };
    };
in
{
  security.acme = {
    acceptTerms = true;
    defaults.email = "paho@paholg.com";
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;

    # Strip @domain from oauth2-proxy usernames for Foundry
    # (e.g., "guest@auth.paholg.com" -> "guest")
    commonHttpConfig = ''
      map $preferred_username $username_short {
        ~^(?<name>[^@]+)@  $name;
        default            $preferred_username;
      }
    '';

    virtualHosts."tv.paholg.com" = {
      enableACME = true;
      forceSSL = true;
      locations."/".proxyPass = "http://localhost:${toString config.custom.ports.jellyfin}";
    };

    virtualHosts."vtt.paholg.com" = {
      enableACME = true;
      forceSSL = true;
      locations = mkAuthLocation {
        location = "/";
        proxyPass = "http://localhost:${toString config.custom.ports.foundry}";
        groups = [
          "foundry_vtt_player@auth.paholg.com"
          "foundry_vtt_admin@auth.paholg.com"
        ];
        signInUrl = "https://home.paholg.com/oauth2/sign_in";
        headers = ''
          proxy_set_header X-Auth-Request-Preferred-Username $username_short;
          proxy_set_header X-Auth-Request-Groups $groups;
        '';
        extraConfig = ''
          # Disable proxy buffering for faster streaming
          proxy_buffering off;

          # Increase max body size for asset uploads
          client_max_body_size 300M;
        '';
      };
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

              # oauth2-proxy sends large headers (JWT tokens, cookies)
              proxy_buffer_size 128k;
              proxy_buffers 4 256k;
              proxy_busy_buffers_size 256k;
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
        (mkAuthLocation {
          location = "/prowlarr";
          proxyPass = "http://127.0.0.1:${toString config.custom.ports.prowlarr}/prowlarr";
          groups = [ "arr_admin@auth.paholg.com" ];
        })
        (mkAuthLocation {
          location = "/radarr";
          proxyPass = "http://127.0.0.1:${toString config.custom.ports.radarr}/radarr";
          groups = [ "arr_admin@auth.paholg.com" ];
        })
        (mkAuthLocation {
          location = "/sonarr";
          proxyPass = "http://127.0.0.1:${toString config.custom.ports.sonarr}/sonarr";
          groups = [ "arr_admin@auth.paholg.com" ];
        })
        (mkAuthLocation {
          location = "/transmission";
          proxyPass = "http://${config.custom.ips.vpn_veth}:${toString config.custom.ports.transmission}";
          groups = [ "arr_admin@auth.paholg.com" ];
        })
        (mkAuthLocation {
          location = "/rtorrent/";
          proxyPass = "http://${config.custom.ips.vpn_veth}:${toString config.custom.ports.rtorrent}/rtorrent/";
          groups = [ "arr_admin@auth.paholg.com" ];
        })
        (mkAuthLocation {
          location = "/zigbee";
          proxyPass = "http://127.0.0.1:${toString config.custom.ports.zigbee_frontend}/zigbee";
          groups = [ "home_assistant_admin@auth.paholg.com" ];
        })
      ];
    };

    virtualHosts."bitmagnet.paholg.com" = {
      enableACME = true;
      forceSSL = true;

      locations = mkAuthLocation {
        location = "/";
        proxyPass = "http://${config.custom.ips.vpn_veth}:${toString config.custom.ports.bitmagnet}";
        groups = [ "arr_admin@auth.paholg.com" ];
      };
    };

    # Internal rtorrent RPC for sonarr/radarr
    virtualHosts."localhost:${toString config.custom.ports.rtorrent_scgi}" = {
      listen = [{ addr = "127.0.0.1"; port = config.custom.ports.rtorrent_scgi; }];
      locations."/".extraConfig = ''
        scgi_pass unix:${config.services.rtorrent.rpcSocket};
        include ${pkgs.nginx}/conf/scgi_params;
      '';
    };
  };
}
