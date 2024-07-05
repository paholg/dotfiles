{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.custom.media;

  group = "media";
in
{
  options.custom.media = {
    enable = mkEnableOption "Media Serving";
    gid = mkOption { type = types.int; };

    ports = mkOption { type = types.attrsOf types.int; };

    container_ip = mkOption { type = types.attrsOf types.str; };

    storage = mkOption { type = types.attrsOf types.path; };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      80
      443
    ];

    users.groups.media = {
      gid = cfg.gid;
      members = [ "paho" ];
    };

    security.acme = {
      acceptTerms = true;
      defaults.email = "paho@paholg.com";
    };

    services = {
      nginx = {
        enable = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;

        virtualHosts."10.0.0.4" = {
          locations."/transmission".proxyPass = "http://${cfg.container_ip}:${cfg.ports.transmission}";
          locations."/prowlarr".proxyPass = "http://localhost:${cfg.ports.prowlarr}/prowlarr";
          locations."/radarr".proxyPass = "http://localhost:${cfg.ports.radarr}/radarr";
          locations."/sonarr".proxyPass = "http://localhost:${cfg.ports.sonarr}/sonarr";
        };

        virtualHosts."tv.paholg.com" = {
          enableACME = true;
          forceSSL = true;
          # jellyfin
          locations."/".proxyPass = "http://localhost:${cfg.ports.jellyfin}";
        };
      };

      jellyfin = {
        enable = true;
        inherit group;
      };

      plex = {
        enable = true;
        inherit group;
        package = pkgs.unfree.plex;
        openFirewall = true;
        dataDir = storage + /plex;
      };

      prowlarr = {
        enable = true;
        openFirewall = true;
      };

      radarr = {
        enable = true;
        inherit group;
        openFirewall = true;
        dataDir = storage + /radarr;
      };

      sonarr = {
        enable = true;
        inherit group;
        openFirewall = true;
        dataDir = storage + /sonarr;
      };
    };
  };
}
