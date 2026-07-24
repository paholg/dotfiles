{
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./niri.nix
    ./security.nix
  ];

  config = {
    hardware = {
      bluetooth.enable = true;
      bluetooth.powerOnBoot = true;
      graphics.enable = true;
      graphics.enable32Bit = true;
    };

    location.provider = "geoclue2";

    environment.sessionVariables = {
      GTK_DATA_PREFIX = [ "${config.system.path}" ];
    };

    programs.dconf.enable = true;

    fonts.packages = [ pkgs.nerd-fonts.fira-code ];

    # Pipewire audio
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      # Zoom requests 480-sample (10 ms) buffers; power-of-two rounding drives
      # the graph at 256 samples (5.3 ms), causing xruns (pops) on the USB DAC.
      # Floor Zoom's streams at 1024 samples (21 ms) — the added ~16 ms is
      # negligible next to network + jitter-buffer latency in a call.
      extraConfig.pipewire-pulse."92-zoom-min-quantum" = {
        "pulse.rules" = [
          {
            matches = [ { "application.process.binary" = "zoom"; } ];
            actions.update-props = {
              "pulse.min.quantum" = "1024/48000";
            };
          }
        ];
      };
    };

    hardware.printers.ensurePrinters = [
      {
        name = "Samsung_Xpress";
        location = "Paho Office";
        deviceUri = "ipp://10.0.0.2/ipp";
        model = "samsung/M267x.ppd";
      }
    ];

    services.udev.extraRules =
      # Disable USB autosuspend on Audioengine HD3 to prevent broken pipe
      # loops when the device wakes from suspend mid-stream.
      ''
        ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="0a12", ATTR{idProduct}=="1243", ATTR{power/control}="on", ATTR{power/autosuspend_delay_ms}="-1"
      ''
      # Insta360 Link 2: autosuspend causes EPROTO and USB disconnect mid-stream.
      # Same fix as upstream kernel applies to Link 1 (UVC_QUIRK_DISABLE_AUTOSUSPEND).
      + ''
        ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="2e1a", ATTR{idProduct}=="4c04", ATTR{power/control}="on", ATTR{power/autosuspend_delay_ms}="-1"
      '';

    services = {
      blueman.enable = true;

      libinput = {
        enable = true;
        touchpad = {
          tapping = false;
          clickMethod = "clickfinger";
          disableWhileTyping = true;
        };
      };

      printing = {
        enable = true;
        drivers = [ pkgs.samsung-unified-linux-driver ];
      };
    };
  };
}
