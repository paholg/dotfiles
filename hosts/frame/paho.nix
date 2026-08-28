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
    # Skip the embedded Chromium.
    disableCef = true;
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
    serve = {
      body = # fish
        ''
          ~/src/scholarly/scratches/worktree-login &
          x dev
        '';
    };
  };

  programs.ssh.settings = {
    beelink = {
      User = "pluriegregg@scholarlysoftware.com";
      HostName = "scholarly-seattle-beelink";
    };
  };

  home.shellAliases = {
    charge-limit = "sudo framework_tool --charge-limit";
    my = "mycli -h $DATABASE_HOST -uroot -D scholarly_development";
    red = ''redis-cli -h "$(devconcurrent show workspace)".redis.test'';
  };

  home.sessionVariables = {
    NGROK_URL = "paholg.ngrok.app";
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
    conf="$HOME/.config/zoomus.conf"
    if [ ! -f "$conf" ]; then
      touch "$conf"
    fi
    set_zoom() { ${pkgs.crudini}/bin/crudini --set "$conf" "$@"; }
    ${zoomActivationScript}
  '';

  home.packages = (
    with pkgs;
    [
      awscli2
      external.claude-desktop
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
      ssm-session-manager-plugin # AWS plugin
      tailscale
      terraform
      (zoom-us.override { gnomeXdgDesktopPortalSupport = true; })
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
        --additional-packages "systemd libpam-systemd" \
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
