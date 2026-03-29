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

      # The Audioengine HD3 uses a CSR Bluetooth chip (0a12:1243) whose USB
      # product string resolves to "CSRA64210 [TaoTronics Headset BH-22 ...]"
      # in the USB ID database. Override so apps show the real name.
      wireplumber.extraConfig."50-audioengine-hd3" = {
        "monitor.alsa.rules" = [
          {
            matches = [
              { "device.bus-id" = "usb-Audioengine_Audioengine_HD3_B40020170802-00"; }
            ];
            actions.update-props = {
              "device.description" = "Audioengine HD3";
              "api.acp.auto-profile" = false;
              "device.profile" = "output:analog-stereo";
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

    # Disable USB autosuspend on audio devices to prevent broken pipe
    # loops when the device wakes from suspend mid-stream.
    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="0a12", ATTR{idProduct}=="1243", ATTR{power/control}="on", ATTR{power/autosuspend_delay_ms}="-1"
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
