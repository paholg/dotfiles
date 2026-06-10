{ lib, pkgs, ... }:
let
  jsonFormat = pkgs.formats.json { };

  markUrgent = msg: {
    hooks = [
      {
        type = "command";
        command = "echo $KITTY_PID 'claude: ${msg}' | nc -U /run/user/1000/mark-urgent.sock";
      }
    ];
  };

  # Merged into ~/.claude/settings.json at activation, leaving the rest of the
  # file for Claude Code to manage at runtime. The file gets bind-mounted into
  # devcontainers, so it must be a plain file with no nix store references.
  managedSettings = {
    hooks = {
      Notification = [ (markUrgent "notification") ];
      PermissionRequest = [ (markUrgent "permission request") ];
      TaskCompleted = [ (markUrgent "task completed") ];
    };
    statusLine = {
      type = "command";
      command = "bash ~/.claude/statusline-command.sh";
    };
  };

  staticSettings = jsonFormat.generate "claude-managed-settings.json" managedSettings;
in
{
  home.activation.claudeSettings =
    lib.hm.dag.entryAfter [ "writeBoundary" ] # bash
      ''
        settings="$HOME/.claude/settings.json"
        mkdir -p "$HOME/.claude"
        install -m 644 ${./statusline-command.sh} "$HOME/.claude/statusline-command.sh"
        if [ ! -e "$settings" ]; then
          echo '{}' > "$settings"
        fi
        merged="$(${lib.getExe pkgs.jq} -n '$dynamic[0] * $static[0]' \
          --slurpfile dynamic "$settings" --slurpfile static ${staticSettings})"
        printf '%s\n' "$merged" > "$settings"
      '';
}
