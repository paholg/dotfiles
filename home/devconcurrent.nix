{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.custom.devconcurrent;
  toml = pkgs.formats.toml { };

  # Nix attrsets (and `builtins.toJSON`) always sort keys, but devconcurrent cares about project
  # order (the first is the default). So we render each project as its own TOML fragment and
  # concatenate them in list order.
  projectFile =
    project:
    toml.generate "devconcurrent-${project.name}.toml" {
      projects.${project.name} = removeAttrs project [ "name" ];
    };
in
{
  options.custom.devconcurrent = {
    projects = lib.mkOption {
      description = "Projects in order; the first is devconcurrent's default.";
      type = lib.types.listOf (
        lib.types.submodule {
          freeformType = toml.type;
          options.name = lib.mkOption { type = lib.types.str; };
        }
      );
      default = [ ];
    };
    worktree = lib.mkOption {
      type = toml.type;
      default = { };
    };
  };

  config = lib.mkIf (cfg.projects != [ ]) {
    assertions = [
      {
        assertion = lib.allUnique (map (p: p.name) cfg.projects);
        message = "custom.devconcurrent.projects has duplicate names";
      }
    ];

    xdg.configFile."devconcurrent/config.toml".source = pkgs.concatText "devconcurrent.toml" (
      [
        (toml.generate "devconcurrent-base.toml" {
          inherit (cfg) worktree;

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
        })
      ]
      ++ map projectFile cfg.projects
    );
  };
}
