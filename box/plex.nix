{ pkgs, ... }:

{
  services.plex = {
    enable = true;
    openFirewall = true;
    dataDir = "/mnt/storage/plex";
    package = pkgs.plex.overrideAttrs (x:
      let
        version = "1.22.0.4163-d8c4875dd";
        sha1 = "ff4e43e2d666b9f5e071e3921bb8f9af76bb0a4d";
      in {
        name = "plex-${version}";
        src = pkgs.fetchurl {
          url =
            "https://downloads.plex.tv/plex-media-server-new/${version}/redhat/plexmediaserver-${version}.x86_64.rpm";
          inherit sha1;
        };
      });
  };
}
