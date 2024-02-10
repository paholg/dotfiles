{pkgs, ...}: {
  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      general = {
        border_size = 6;
        gaps_in = 0;
        gaps_out = 0;

        "col.active_border" = "rgba(00ffff80)";
        "col.inactive_border" = "rgba(00000000)";

        resize_on_border = true;
      };

      decoration = {
        rounding = 8;
      };

      input = {
        repeat_delay = 200;
        repeat_rate = 50;
      };

      exec-once = [
        "waybar"
        "firefox"
        "steam"
      ];

      misc = {
        mouse_move_enables_dpms = true;
        key_press_enables_dpms = true;
      };

      windowrulev2 = [
        "workspace 1 silent, class:(steam)"
        "workspace 2 silent, class:(firefox)"
        "workspace 9 silent, title:(Wine System Tray)"
        # "fullscreen, class:^(steam_app)(.*)$"
        "noborder, onworkspace:1"
      ];

      bindm = [
        "SUPER, mouse:272, movewindow"
        "SUPER, mouse:273, resizewindow"
      ];

      bind = let
        set_sink = name: "list_sinks | jq '.\"${name}\"' | xargs wpctl set-default";
      in
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

          "SUPER, H, exec, ${set_sink "HDA Intel PCH"}"
          "SUPER, S, exec, ${set_sink "Audioengine HD3"}"
        ]
        ++ builtins.concatMap (x: let
          ws = toString x;
        in [
          "SUPER, ${ws}, workspace, ${ws}"
          "SUPER SHIFT, ${ws}, movetoworkspace, ${ws}"
        ]) [1 2 3 4 5 6 7 8 9]
        ++ (
          let
            couch = bind: [
              "${bind}, exec, hyprctl keyword monitor DP-3, disable"
              "${bind}, exec, hyprctl keyword monitor HDMI-A-1, 3840x2160@120, 0x0, 2"
              "${bind}, exec, ${set_sink "HDA ATI HDMI"}"
            ];
            desk = bind: [
              "${bind}, exec, hyprctl keyword monitor HDMI-A-1, disable"
              "${bind}, exec, hyprctl keyword monitor DP-3, 3840x2160@144, 0x0, 1"
              "${bind}, exec, ${set_sink "Audioengine HD3"}"
            ];
          in
            # Couch mode
            (let bind = "SUPER, M"; in (couch bind))
            # Desk mode
            ++ (let bind = "SUPER SHIFT, M"; in (desk bind))
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
  programs.eww = {
    enable = true;
    configDir = ./eww;
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
    anyrun # App launcher
    mangohud # GPU perforance overlay
    qt5.qtwayland # Qt wayland support (recommended by hyprland)
    qt6.qtwayland # Qt wayland support (recommended by hyprland)
    tofi # App launcher
    wl-clipboard
    wlr-randr # xrandr for wl-roots
    waybar # status bar
  ];
}
