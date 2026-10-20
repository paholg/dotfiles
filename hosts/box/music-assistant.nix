{ config, ... }:
let
  uid = toString config.custom.uids.music_assistant;
  gid = toString config.custom.groups.music_assistant;
in
{
  users.groups.music-assistant = {
    gid = config.custom.groups.music_assistant;
  };

  users.users.music-assistant = {
    isSystemUser = true;
    group = "music-assistant";
    uid = config.custom.uids.music_assistant;
  };

  # The official image bundles the Tidal credentials; the nixpkgs build does not.
  virtualisation.oci-containers.containers.music-assistant = {
    user = "${uid}:${gid}";
    image = "ghcr.io/music-assistant/server:2.10.7";
    volumes = [ "${config.custom.drives.data}/music-assistant:/data" ];
    # Required for player discovery and streaming:
    # https://www.music-assistant.io/installation/
    extraOptions = [ "--network=host" ];
  };

  # Stream and sendspin ports, as opened by the nixpkgs module's openFirewall.
  networking.firewall.allowedTCPPorts = with config.custom.ports; [
    music_assistant_stream
    music_assistant_sendspin
  ];

  services.nginx.virtualHosts."music.paholg.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.custom.ports.music_assistant}";
      proxyWebsockets = true;
    };
  };
}
