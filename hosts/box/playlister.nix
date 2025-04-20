{
  config,
  lib,
  pkgs,
  ...
}:
let
  envPath = config.age.secrets.playlister_env.path;
  cacheDir = "/home/paho/.cache/playlister";
in
{
  age.secrets.playlister_env.file = ../../secrets/playlister_env;

  systemd.services.playlister = {
    description = "Update playlists from r/listentothis";
    serviceConfig = {
      Type = "oneshot";
      EnvironmentFile = envPath;
    };
    environment.CACHE_DIR = cacheDir;
    script = lib.getExe pkgs.external.playlister;
  };

  systemd.timers.playlister = {
    description = "Update playlists from r/listentothis hourly";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* *:00:00";
    };

  };
}
