{ config, pkgs, ... }:
let
  cfg = config.custom;
in
{
  config = {
    networking.firewall.allowedTCPPorts = [
      80
      443
    ];

    users.groups.media = {
      gid = cfg.groups.media;
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

        virtualHosts."10.0.0.4" = {
          locations."/transmission".proxyPass = "http://${cfg.ips.container}:${toString cfg.ports.transmission}";
          locations."/prowlarr".proxyPass = "http://localhost:${toString cfg.ports.prowlarr}/prowlarr";
          locations."/radarr".proxyPass = "http://localhost:${toString cfg.ports.radarr}/radarr";
          locations."/sonarr".proxyPass = "http://localhost:${toString cfg.ports.sonarr}/sonarr";
        };

        virtualHosts."auth.paholg.com" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://localhost:${toString config.services.keycloak.settings.http-port}/";
        };

        virtualHosts."tv.paholg.com" = {
          enableACME = true;
          forceSSL = true;
          locations."/".proxyPass = "http://localhost:${toString cfg.ports.jellyfin}";
        };
      };

      postgresql = {
        enable = true;
        package = pkgs.postgresql_16;
        dataDir = config.custom.drives.storage + "/postgres";
      };

      keycloak = {
        enable = true;
        database = {
          type = "postgresql";
          passwordFile = config.age.secrets.keycloak_postgres_pw.path;
        };

        settings = {
          hostname = "auth.paholg.com";
          http-port = 38080;
          proxy-headers = "xforwarded";
          http-enabled = true;
        };
      };

      jellyfin = {
        enable = true;
        group = "media";
      };

      plex = {
        enable = true;
        group = "media";
        package = pkgs.unfree.plex;
        openFirewall = true;
        dataDir = cfg.drives.storage + "/plex";
      };

      prowlarr = {
        enable = true;
        openFirewall = true;
      };

      radarr = {
        enable = true;
        group = "media";
        openFirewall = true;
        dataDir = cfg.drives.storage + "/radarr";
      };

      sonarr = {
        enable = true;
        group = "media";
        openFirewall = true;
        dataDir = cfg.drives.storage + "/sonarr";
      };
    };
  };
}
