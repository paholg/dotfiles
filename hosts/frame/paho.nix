{
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
      '';
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
      "99-zoom" = {
        name = "zoom";
        open-on-output = "eDP-1";
      };
    };
    window-rules = [
      {
        # For selenium tests
        matches = [ { app-id = "chromium-browser"; } ];
        open-floating = true;
      }
    ];
  };

  home.shellAliases = {
    charge-limit = "sudo framework_tool --charge-limit";
    my = "mycli --socket /tmp/mysql.sock -uroot -D scholarly_development";
  };

  home.sessionVariables = {
    NGROK_URL = "paholg.ngrok.app";
    PODMAN_COMPOSE_WARNING_LOGS = "false";
  };

  programs.obs-studio = {
    enable = true;
    plugins = [ pkgs.obs-studio-plugins.obs-backgroundremoval ];
  };

  home.packages =
    (with pkgs; [
      csvtool
      distrobox
      dive # look into docker image layers
      framework-tool
      heroku
      # iredis
      mermaid-cli
      mycli
      mysql80
      podman-compose
      podman-tui
      pscale
      redis
      terraform
      terraform-ls
      vscodium-fhs
      zoom-us
    ])
    ++ [
      zoomWc
    ];

  # home.file.".config/zoomus.conf".source = ./zoom.conf;

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
