{...}: {
  services.plex = {
    enable = true;
    openFirewall = true;
    dataDir = "/mnt/storage/plex";
  };
}
