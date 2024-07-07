{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.custom;

  # TODO: get this bin dir in PATH for hyprland
  set_sink = name: "$HOME/dotfiles/bin/list_sinks | jq '.\"${name}\"' | xargs wpctl set-default";

  couch_mode = [
    "hyprctl keyword monitor DP-2, disable"
    "hyprctl keyword monitor DP-3, disable"
    "hyprctl keyword monitor HDMI-A-1, 3840x2160@60, 0x0, 2"
    "${set_sink "HDA ATI HDMI"}"
    "hyprctl dispatch, systemctl --user stop swayidle.service"
  ];
  game_mode = [
    "hyprctl keyword monitor HDMI-A-1, disable"
    "hyprctl keyword monitor DP-2, disable"
    "hyprctl keyword monitor DP-3, 3840x2160@144, 0x0, 1"
    "hyprctl dispatch, systemctl --user start swayidle.service"
  ];
  work_mode = [
    "hyprctl keyword monitor HDMI-A-1, disable"
    "hyprctl keyword monitor DP-2, 3840x2160@60, 2160x0, 1, transform,3"
    "hyprctl keyword monitor DP-3, 3840x2160@144, 0x0, 1, transform,1"
    "hyprctl dispatch, systemctl --user start swayidle.service"
  ];
in
{
  options.custom.hyprland = {
    enable = mkEnableOption "Hyprland";
  };

  config = mkIf cfg.hyprland.enable {
    wayland.windowManager.hyprland = {
      enable = true;
      xwayland.enable = true;
      systemd.enable = true;
      settings = {
        general = {
          border_size = 6;
          gaps_in = 0;
          gaps_out = 0;

          "col.active_border" = "rgba(00ffff80)";
          "col.inactive_border" = "rgba(00000000)";

          # resize_on_border = true;
        };

        decoration = {
          rounding = 8;
        };

        input = {
          repeat_delay = 200;
          repeat_rate = 50;
        };

        dwindle = {
          preserve_split = true;
        };

        exec-once = [
          "waybar"
          "firefox"
          "steam-gamescope"
        ] ++ game_mode;

        misc = {
          mouse_move_enables_dpms = true;
          key_press_enables_dpms = true;
        };

        monitor = [
          # Keep from tv turning on to be auto-detected.
          "HDMI-A-1, disabled"
        ];

        windowrulev2 = [
          "workspace 1 silent, class:(steam)"
          "workspace 2 silent, class:(firefox)"
          "workspace 3 silent, class:(discord)"
          "workspace 3 silent, class:(steam) title:(Friends List.*)"
          "workspace 9 silent, title:(Wine System Tray)"
          "noborder, onworkspace:1"
        ];

        bindm = [
          "SUPER, mouse:272, movewindow"
          "SUPER, mouse:273, resizewindow"
        ];

        bind =
          [
            "SUPER, F, exec, firefox"
            "SUPER, T, exec, alacritty"
            "SUPER, R, exec, tofi-run | xargs hyprctl dispatch exec --"

            "SUPER, O, exec, pavucontrol"

            "SUPER, SPACE, fullscreen"
            "SUPER, TAB, cyclenext"

            "SUPER, Q, killactive"
            "SUPER, Z, togglefloating"
            "SUPER, N, exec, sleep 1; hyprctl dispatch dpms off"

            "SUPER, H, exec, ${set_sink "KT USB Audio"}"
            "SUPER, S, exec, ${set_sink "Audioengine HD3"}"

            "SUPER, J, layoutmsg, togglesplit"
          ]
          ++ builtins.concatMap
            (
              x:
              let
                ws = toString x;
              in
              [
                "SUPER, ${ws}, focusworkspaceoncurrentmonitor, ${ws}"
                "SUPER SHIFT, ${ws}, movetoworkspace, ${ws}"
              ]
            )
            [
              1
              2
              3
              4
              5
              6
              7
              8
              9
            ]
          ++ (
            let
              switch = bind: map (cmd: "${bind}, exec, ${cmd}");
            in
            switch "SUPER, M" couch_mode
            ++ switch "SUPER CONTROL, M" game_mode
            ++ switch "SUPER SHIFT, M" work_mode
          );
      };
    };

    # App launcher
    # programs.anyrun = {
    #   enable = true;
    #   config = {};
    # };

    # TODO: FIXME.
    # TODO: If continuing to use waybar, check in its config.
    # Status bar.
    # programs.eww = {
    #   enable = true;
    #   configDir = ./eww;
    # };

    programs.mangohud = {
      enable = true;
      enableSessionWide = true;
      settings = {
        toggle_hud = "Super_L+slash";

        fps_limit = 0;
      };
    };

    home.sessionVariables = {
      # Hide mangohud by default
      MANGOHUD_CONFIG = "no_display";
    };

    # Notification daemon.
    services.mako = {
      enable = true;
    };

    # Idle management
    services.swayidle = {
      enable = true;
      timeouts = [
        {
          timeout = 600;
          command = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
        }
      ];
    };

    home.packages = with pkgs; [
      # anyrun # App launcher
      qt5.qtwayland # Qt wayland support (recommended by hyprland)
      qt6.qtwayland # Qt wayland support (recommended by hyprland)
      tofi # App launcher
      wl-clipboard
      wlr-randr # xrandr for wl-roots
      waybar # status bar
    ];
  };
}
