{
  enable = true;
  systemd.enable = true;
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

    memory = {
      format = "{avail:0.1f} G";
    };
    "niri/workspaces" = { };
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
}
