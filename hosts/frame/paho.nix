{
  lib,
  pkgs,
  ...
}:
let
  zoomWc = pkgs.writeShellApplication {
    name = "zoom-watercooler";
    runtimeInputs = with pkgs; [
      jq
      niri
    ];
    text = # bash
      ''
        niri msg -j windows \
          | jq '.[] | select(.app_id == "Zoom Workplace") | .pid' \
          | head -n1 \
          | xargs kill || true

        xargs xdg-open < "$HOME/docs/watercooler_link"
      '';
  };

  zoomSettings = {
    enableWaylandShare = false;
    xwayland = true;
    enableMiniWindow = false;
    autoScale = true;
    captureHDCamera = true;
    enableAlphaBuffer = true;
    playSoundForNewMessage = false;
    showSystemTitlebar = false;
    noSandbox = true;
  };

  zoomActivationScript = lib.concatStringsSep "\n" (
    lib.mapAttrsToList
      (key: value: ''set_zoom General ${key} ${lib.generators.mkValueStringDefault { } value}'')
      zoomSettings
  );
in
{
  imports = [
    ../../home
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
        COMPLETE=fish devconcurrent | source
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
              kitty --detach fish -c "dc_exec_in_ws $name"; or return
              notify-send "workspace $name ready"
              terminal-mark-urgent
            case destroy
              niri msg action focus-workspace $name; or return
              niri msg action unset-workspace-name; or return
              dc $argv
            case '*'
              echo "Usage: ws {up|destroy} NAME [ARGS...]"
              return 1
          end
        '';
    };
  };

  programs.niri.settings = {
    binds = {
      "Super+Ctrl+W".action.spawn = "zoom-watercooler";
    };
    outputs = {
      "DP-3" = {
        enable = true;
        position = {
          x = 1735;
          y = 0;
        };
      };
      "eDP-1" = {
        enable = true;
        position = {
          x = 0;
          y = 0;
        };
        scale = 1.3;
      };
    };
    spawn-at-startup = [
      { command = [ "firefox" ]; }
      { command = [ "slack" ]; }
    ];
    workspaces = {
      "01-main".open-on-output = "DP-3";
      "02-chat".open-on-output = "eDP-1";
    };
    window-rules = [
      {
        # For selenium tests
        matches = [ { app-id = "chromium-browser"; } ];
        open-floating = true;
      }
    ];
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
    my = "mysql --socket sockets/mysql/mysqld.sock -uroot -D scholarly_development";
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
    conf="$HOME/.config/zoomus.conf"
    if [ ! -f "$conf" ]; then
      touch "$conf"
    fi
    set_zoom() { ${pkgs.crudini}/bin/crudini --set "$conf" "$@"; }
    ${zoomActivationScript}
  '';

  home.packages =
    (with pkgs; [
      csvtool
      devcontainer
      docker
      distrobox
      dive # look into docker image layers
      external.devconcurrent
      external.envswitch
      framework-tool
      heroku
      # iredis
      mermaid-cli
      mycli
      mysql80
      pscale
      redis
      terraform
      terraform-ls
      zoom-us
    ])
    ++ [
      zoomWc
    ];

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
