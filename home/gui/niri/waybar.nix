{ ... }:
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    style = ./waybar.css;
    settings.mainBar = {
      layer = "top";
      position = "top";
      modules-left = [
        "niri/workspaces"
        "niri/window"
      ];
      modules-center = [
        "cpu"
        "memory"
        "temperature"
        "network"
      ];
      modules-right = [
        "pulseaudio"
        "battery"
        "clock"
        "tray"
      ];

      cpu = {
        interval = 1;
        states = {
          okay = 20;
          warn = 50;
          crit = 80;
        };
      };
      memory = {
        format = "{avail:0.1f} G";
        states = {
          okay = 50;
          warn = 65;
          crit = 80;
        };
      };
      battery = {
        interval = 1;
        states = {
          okay = 50;
          warn = 25;
          crit = 15;
        };
        format = "{capacity}% <span foreground='#ff0000'>*</span>";
        format-charging = "{capacity}% <span foreground='#00ff00'>+</span>";
        format-discharging = "{capacity}% <span foreground='#ff0000'>-</span>";
        format-full = "{capacity}%  ";
      };
      network = {
        "format-wifi" = "{essid} ({signalStrength}%) ";
        "format-ethernet" = "{ipaddr}";
        "format-disconnected" = "Disconnected ⚠";
      };
      clock = {
        format = "{:%F %H:%M:%S}";
        interval = 1;
        timezones = [
          "America/Los_Angeles"
          "Etc/UTC"
        ];
      };
    };
  };
}
