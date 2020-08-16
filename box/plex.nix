{ pkgs, ... }:

{
  services.plex = {
    enable = true;
    openFirewall = true;
    dataDir = "/mnt/storage/plex";
    package = pkgs.plex.overrideAttrs (x:
      let
        version = "1.20.0.3181-0800642ec";
        sha1 = "4d090a3f95193052382325a54dc3356cef7e253b";
      in {
        name = "plex-${version}";
        src = pkgs.fetchurl {
          url =
            "https://downloads.plex.tv/plex-media-server-new/${version}/redhat/plexmediaserver-${version}.x86_64.rpm";
          inherit sha1;
        };
      });
  };

  # TODO: Add ddns
  # TODO: Add nginx
}
