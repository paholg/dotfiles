{pkgs, ...}: {
  networking.firewall.allowedTCPPorts = [80 443];

  users.groups.media = {
    gid = 1100;
    members = ["paho"];
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
        locations."/transmission".proxyPass = "http://10.233.1.2:9091";
        locations."/prowlarr".proxyPass = "http://localhost:9696/prowlarr";
        locations."/radarr".proxyPass = "http://localhost:7878/radarr";
        locations."/sonarr".proxyPass = "http://localhost:8989/sonarr";
      };

      virtualHosts."tv.paholg.com" = {
        enableACME = true;
        forceSSL = true;
        # jellyfin
        locations."/".proxyPass = "http://localhost:8096";
      };
    };

    jellyfin = {
      enable = true;
      group = "media";
    };

    plex = {
      enable = true;
      package = pkgs.unfree.plex;
      openFirewall = true;
      dataDir = "/mnt/storage/plex";
      group = "media";
    };

    prowlarr = {
      enable = true;
      openFirewall = true;
    };

    radarr = {
      enable = true;
      openFirewall = true;
      dataDir = "/mnt/storage/radarr";
      group = "media";
    };

    sonarr = {
      enable = true;
      openFirewall = true;
      dataDir = "/mnt/storage/sonarr";
      group = "media";
    };
  };
}
