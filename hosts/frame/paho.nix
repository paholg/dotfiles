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
      ../../home/gui/niri/base.kdl
      ../../home/gui/niri/paho.kdl
      ../../home/gui/niri/binds.kdl
      ./niri.kdl
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
    dc_exec_in_ws = {
      body = # fish
        ''
          niri msg action move-window-to-workspace --focus=false $argv[1]; or return
          dc exec $argv[1] /bin/zsh -lc 'TRAPINT() { return 0 }; bin/dev; exec /bin/zsh -l'
          exec fish
        '';
    };

    ws = {
      wraps = "dc";
      body = # fish
        ''
          set -l cmd $argv[1]
          set -l name $argv[2]

          switch $cmd
            case up
              niri msg action set-workspace-name $name; or return
              dc $argv; or return
              cd ~/src/scholarly/.worktrees/$name; or return
              direnv allow; or return
              eval (direnv export fish); or return
              echo "$KITTY_PID" | nc -U /run/user/1000/mark-urgent.sock
              dc x
            case destroy
              niri msg action unset-workspace-name; or return
              dc $argv; or return
              exit
            case '*'
              echo "Usage: ws {up|destroy} NAME [ARGS...]"
              return 1
          end
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
    envswitch = {
      description = "Show which envswitch environment is currently active.";
      command = "envswitch get";
      when = "envswitch get";
      style = "yellow";
      format = "[($symbol $output )]($style)";
      symbol = "";
    };
    dc_ports = {
      description = "Show dc forwarded ports for this workspace";
      command = "devconcurrent show ports";
      when = "devconcurrent show workspace";
      style = "blue";
      format = "[($symbol $output )]($style)";
      symbol = "󰖟";
    };
  };

  home.shellAliases = {
    charge-limit = "sudo framework_tool --charge-limit";
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
      mysql84
      heroku
      pscale
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
