{pkgs, ...}: {
  services.plex = {
    enable = true;
    openFirewall = true;
    dataDir = "/mnt/storage/plex";
    package = pkgs.plex.overrideAttrs (x: let
      version = "1.28.2.6106-44a5bbd28";
      sha1 = "c3daffddac1a6a0f9a94bb3893821aaf0876e8cb";
    in {
      name = "plex-${version}";
      src = pkgs.fetchurl {
        url = "https://downloads.plex.tv/plex-media-server-new/${version}/redhat/plexmediaserver-${version}.x86_64.rpm";
        inherit sha1;
      };
    });
  };
}
