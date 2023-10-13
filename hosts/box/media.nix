{...}: {
  imports = [./jellyfin.nix];

  users.groups.media = {};

  services = {
    plex = {
      enable = true;
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
