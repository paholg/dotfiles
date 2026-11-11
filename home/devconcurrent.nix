{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.devconcurrent;
  toml = pkgs.formats.toml { };
in
{
  options.custom.devconcurrent = {
    primaryProject = lib.mkOption {
      description = "The project to use when none is given or inferred from the current directory.";
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
    projects = lib.mkOption {
      description = "Projects by name.";
      type = lib.types.attrsOf toml.type;
      default = { };
    };
    worktree = lib.mkOption {
      type = toml.type;
      default = { };
    };
  };

  config = {
    custom.devconcurrent.projects.dotfiles.path = "~/dotfiles";

    xdg.configFile."devconcurrent/config.toml".source = toml.generate "devconcurrent.toml" (
      {
        inherit (cfg) projects worktree;

        hooks = {
          beforeUp = ''
            [ -z "$NIRI_SOCKET" ] && exit
            niri msg --json workspaces | jq -e '.[] | select(.is_focused) | .name != null' >/dev/null && exit
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
      }
      // lib.optionalAttrs (cfg.primaryProject != null) { inherit (cfg) primaryProject; }
    );
  };
}
