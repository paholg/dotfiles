{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.devconcurrent;
in
{
  options.custom.devconcurrent = {
    projects = lib.mkOption {
      type = (pkgs.formats.toml { }).type;
      default = { };
    };
  };

  config = lib.mkIf (cfg.projects != { }) {
    xdg.configFile."devconcurrent/config.toml".source =
      (pkgs.formats.toml { }).generate "devconcurrent.toml"
        {
          inherit (cfg) projects;

          hooks = {
            beforeUp = ''
              [ -z "$NIRI_SOCKET" ] && exit
              niri msg action set-workspace-name "$DEVCONCURRENT_WORKSPACE" &&
                fish -c 'ws track "$DEVCONCURRENT_WORKSPACE_PATH"'
            '';
            afterUp = ''
              [ -z "$NIRI_SOCKET" ] || mark-urgent
            '';
            afterDestroy = ''
              fish -c 'ws untrack "$DEVCONCURRENT_WORKSPACE"'
            '';
          };
        };
  };
}
