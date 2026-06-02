{
  lib,
  pkgs,
  ...
}:
let
  zoomSettings = {
    enableWaylandShare = true;
    xwayland = false;
    enableMiniWindow = false;
    captureHDCamera = true;
    showSystemTitlebar = false;
  };

  zoomActivationScript = lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      key: value: "set_zoom General ${key} ${lib.generators.mkValueStringDefault { } value}"
    ) zoomSettings
  );

  fixWs = pkgs.writeShellApplication {
    name = "fix-ws";
    runtimeInputs = with pkgs; [
      niri
      jq
    ];
    text = ''
      connected_outputs=$(niri msg --json outputs | jq '[to_entries[] | select(.value.logical != null)]')

      # Chat lives on the RTK 0x0101 monitor if present, else the laptop's internal panel.
      # (make + " " + model is matched together; stringifying the whole value inserts JSON
      # punctuation between the two fields and breaks the "RTK 0x0101" match.)
      chat_monitor=$(echo "$connected_outputs" | jq -r '
        [ .[] | select((.value.make // "") + " " + (.value.model // "") | contains("RTK 0x0101")) ][0].key
        // "eDP-1"
      ')

      main_monitor=$(echo "$connected_outputs" | jq -er --arg chat "$chat_monitor" '
        [ .[] | select(.key != $chat) ][0].key
      ')

      named_workspaces=$(niri msg --json workspaces | jq -r '.[] | select(.name != null) | .name')

      for workspace in $named_workspaces; do
        case "$workspace" in
          chat) target=$chat_monitor ;;
          *)    target=$main_monitor ;;
        esac
        niri msg action move-workspace-to-monitor --reference "$workspace" "$target"
      done
    '';
  };

in
{
  imports = [
    ../../home
    (import ../../home/gui/niri/mkConfig.nix [
      "base.kdl"
      "paho.kdl"
      "binds.kdl"
      "frame.kdl"
    ])
  ];

  home.stateVersion = "24.05";

  custom = {
    username = "paho";
    starship.host_color = "yellow";
    swaylock.color = "224444";
    fish_extra_init =
      # fish
      ''
        set TTY (tty)
        [ "$TTY" = "/dev/tty1" ] && exec "niri-session"
        source /run/agenix/frame_shell_init.sh

        envswitch setup fish | source
      '';
  };

  programs.fish.functions = {
    _dc_env = {
      onEvent = "fish_prompt";
      body = # fish
        ''
          set -l workspace (devconcurrent show workspace); or return
          set -gx REMOTE_HOST $workspace.app.test

          set -gx DATABASE_HOST $workspace.mysql.test

          set -gx REDIS_URL redis://$workspace.redis.test/0
          set -gx REDIS_ACTION_CABLE_URL redis://$workspace.redis.test/1
          set -gx REDIS_TEST_ACTION_CABLE_URL redis://$workspace.redis.test/2
          set -gx REDIS_RACK_ATTACK_URL redis://$workspace.redis.test/9

          set -gx OTEL_EXPORTER_OTLP_ENDPOINT http://$workspace.jaeger.test:4318
        '';
    };

    serve = {
      body = # fish
        ''
          set -l name (dc show workspace); or return

          kitty --detach fish -lC "niri msg action move-window-to-workspace $name --focus false --window-id (get-window-id \$KITTY_PID); and x bin/dev"
          ~/src/scholarly/scratches/worktree-login

          echo "$KITTY_PID" | nc -U /run/user/1000/mark-urgent.sock
        '';
    };
  };

  programs.starship.settings.custom = {
    heroku = {
      command = "echo $HEROKU_APP";
      when = ''[ -n "$HEROKU_APP" ]'';
      format = "[$symbol $output]($style) ";
      style = "#D7BFF2 italic";
      symbol = "";
      shell = "bash";
    };
  };

  home.shellAliases = {
    charge-limit = "sudo framework_tool --charge-limit";
    my = "mycli -h $DATABASE_HOST -uroot -D scholarly_development";
    red = ''redis-cli -h "$(devconcurrent show workspace)".redis.test'';
  };

  home.sessionVariables = {
    NGROK_URL = "paholg.ngrok.app";
    REMOTE_HOST = "paholg.ngrok.app";
  };

  systemd.user.services.ngrok = {
    Unit = {
      Description = "ngrok for rails";
      After = [ "network.target" ];
    };
    Service = {
      ExecStart = "${pkgs.ngrok}/bin/ngrok http 3000 --url paholg.ngrok.app";
      Restart = "on-failure";
      RestartSec = "5s";
    };
    Install.WantedBy = [ "default.target" ];
  };

  programs.obs-studio = {
    enable = true;
    plugins = [ pkgs.obs-studio-plugins.obs-backgroundremoval ];
  };

  home.activation.zoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    conf="$HOME/.var/app/us.zoom.Zoom/config/zoomus.conf"
    if [ ! -f "$conf" ]; then
      mkdir -p "$(dirname "$conf")"
      touch "$conf"
    fi
    set_zoom() { ${pkgs.crudini}/bin/crudini --set "$conf" "$@"; }
    ${zoomActivationScript}
  '';

  xdg.mimeApps.defaultApplications = {
    "x-scheme-handler/zoommtg" = "us.zoom.Zoom.desktop";
    "x-scheme-handler/zoomus" = "us.zoom.Zoom.desktop";
    "x-scheme-handler/zoomphonecall" = "us.zoom.Zoom.desktop";
  };

  home.packages = (
    with pkgs;
    [
      csvtool
      distrobox
      dive # look into docker image layers
      docker
      external.envswitch
      fixWs
      framework-tool
      geckodriver
      mycli
      mysql84
      nss.tools
      heroku
      pscale
      redis
    ]
  );

  programs.vscode = {
    enable = true;
    package = pkgs.vscode.fhs;
  };

  # Vanta stuff
  home.shellAliases.vanta_create = # bash
    ''
      distrobox create -i debian:stable \
        -n vanta \
        -r \
        --home "$HOME/vanta/" \
        --init \
        --additional-packages "systemd libpam-systemd pipewire-audio-client-libraries" \
        --additional-flags "--label keep-true" && \
        vanta_enter \
    '';
  home.shellAliases.vanta_enter = "distrobox enter --root -nw vanta";

  home.file."vanta/check.sh" = {
    executable = true;
    text = "/var/vanta/vanta-cli status";
  };

  home.file."vanta/doctor.sh" = {
    executable = true;
    text = "sudo /var/vanta/vanta-cli doctor";
  };
}
