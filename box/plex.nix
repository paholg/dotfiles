{ pkgs, ... }:

{
  services.plex = {
    enable = true;
    openFirewall = true;
    dataDir = "/mnt/storage/plex";
    package = pkgs.plex.overrideAttrs (x:
      let
        version = "1.23.6.4881-e2e58f321";
        sha1 = "737b004edcd2d4ebe480887d667dd71238b18429";
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
