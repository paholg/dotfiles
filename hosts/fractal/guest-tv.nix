# Guest Steam TV session: gamescope as guest on tty1 via greetd, with
# vt-autopilot switching between it and the desktop on tty2.
{ config, lib, pkgs, ... }:
let
  vt-autopilot = pkgs.writers.writeRubyBin "vt-autopilot" { } (builtins.readFile ./vt-autopilot.rb);
in
{
  users.users.guest = {
    description = "Guest";
    isNormalUser = true;
  };

  programs.steam.gamescopeSession = {
    enable = true;
    # Session-specific; NOT programs.gamescope.args, which would apply to
    # every gamescope invocation (e.g. gamescope-office).
    args = [
      "--adaptive-sync" # VRR support
      "--rt"
      "--hdr-enabled"
      # See /sys/class/drm/card* for output names that gamescope uses.
      "--prefer-output HDMI-A-1"
      # Otherwise the session takes the output's size at startup, which is
      # 640x480 if the TV's EDID read failed.
      "-w 3840"
      "-h 2160"
    ];
    # SteamOS session flags: required for controller navigation of the Steam
    # UI under embedded gamescope. Same set as ChimeraOS's gamescope-session:
    # https://github.com/ChimeraOS/gamescope-session-steam/blob/main/usr/share/gamescope-session-plus/sessions.d/steam
    steamArgs = [
      "-pipewire-dmabuf"
      "-gamepadui"
      "-steamos3"
      "-steampal"
      "-steamdeck"
    ];
    env = {
      STEAM_GAMESCOPE_VRR_SUPPORTED = "1";
      STEAM_GAMESCOPE_HDR_SUPPORTED = "1";
      # wireplumber handles audio device switching.
      STEAM_DISABLE_AUDIO_DEVICE_SWITCHING = "1";
      # Open URLs in Steam's browser; there is no desktop browser here.
      SRT_URLOPEN_PREFER_STEAM = "1";
      # On-screen Steam keyboard.
      QT_IM_MODULE = "steam";
      GTK_IM_MODULE = "Steam";
    };
  };

  services.greetd = {
    enable = true;
    settings.default_session = {
      # greetd points the session's stdout/stderr at the VT, so gamescope's
      # log is lost without systemd-cat.
      command = "${lib.getExe' pkgs.systemd "systemd-cat"} -t steam-gamescope steam-gamescope";
      user = "guest";
    };
  };
  systemd.services.greetd = {
    requires = [ "guest-netns.service" ];
    after = [ "guest-netns.service" ];
    serviceConfig = {
      # Module default is on-success; be robust to nonzero exits too.
      Restart = lib.mkForce "always";
      NetworkNamespacePath = "/run/netns/guest";
      BindReadOnlyPaths = [ "/etc/resolv.conf.guest:/etc/resolv.conf" ];
    };
  };

  systemd.services.vt-autopilot = {
    description = "Switch VTs between guest TV session and desktop";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-logind.service" ];
    path = [
      pkgs.kbd # chvt
      pkgs.procps # pgrep
    ];
    serviceConfig = {
      ExecStart = lib.getExe vt-autopilot;
      Restart = "always";
      RestartSec = 10;
    };
  };

  # Everything behind the TV's USB hub belongs to the guest session only.
  # The Steam udev rules tag controllers `uaccess`, which hands them to
  # whichever session is active; with tty2 active that is paho, whose desktop
  # Steam then grabs the TV controllers and acts on every press at the TV
  # (Big Picture opens, settings change, client restarts). vt-autopilot runs
  # as root and niri gets input via logind, so neither is affected.
  # Must sort after 70-uaccess.rules (adds the tag) and before
  # 73-seat-late.rules (applies the ACL), so it cannot go in extraRules.
  # Matched by USB port: the hub is on bus 3, port 5.1. Moving the TV's cable
  # to another port on the PC breaks this.
  services.udev.packages = [
    (pkgs.writeTextFile {
      name = "tv-input-udev-rules";
      destination = "/etc/udev/rules.d/72-tv-input.rules";
      text = ''
        SUBSYSTEM=="hidraw|input|usb", SUBSYSTEMS=="usb", ATTRS{busnum}=="3", ATTRS{devpath}=="5.1.*", OWNER="guest", MODE="0600", TAG-="uaccess"
      '';
    })
  ];

  # Steam keys a logon session on account + public IP + the client's private
  # LAN IP ("LoginID"), which the client takes from its interfaces, not from
  # the socket it connected with: a per-user source address via policy
  # routing changed nothing. Two clients on one host see the same interfaces,
  # so guest and paho logging into the same account replace each other's
  # session ("Session Replaced"). Run the guest session in its own network
  # namespace with a macvlan on the LAN, so its Steam sees a different
  # address, as the Steam Deck does. macvlan cannot talk to its parent, so
  # guest and the desktop cannot reach each other over the LAN.
  # 10.0.0.x is static hosts, 10.0.1.x APs, 10.0.100.x DHCP.
  # https://github.com/SteamRE/SteamKit/blob/master/SteamKit2/SteamKit2/Steam/Handlers/SteamUser/SteamUser.cs
  systemd.services.guest-netns = {
    description = "Network namespace with a LAN macvlan for the guest session";
    bindsTo = [ "sys-subsystem-net-devices-enp74s0.device" ];
    after = [
      "sys-subsystem-net-devices-enp74s0.device"
      "network-addresses-enp74s0.service"
    ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.iproute2 ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStop = "${lib.getExe' pkgs.iproute2 "ip"} netns del guest";
    };
    script = ''
      # Clean up after an unclean stop.
      ip netns del guest 2>/dev/null || true
      ip link del guest0 2>/dev/null || true
      ip netns add guest
      # Fixed MAC so the router sees one stable device across reboots.
      ip link add guest0 address 02:00:00:0a:00:05 link enp74s0 type macvlan mode bridge
      ip link set guest0 netns guest
      ip -n guest link set lo up
      ip -n guest link set guest0 up
      ip -n guest addr add 10.0.5.5/16 dev guest0
      ip -n guest route add default via 10.0.0.1
    '';
  };
  # systemd-resolved listens on loopback, which is per-namespace.
  environment.etc."resolv.conf.guest".text = lib.concatMapStrings (
    ns: "nameserver ${ns}\n"
  ) config.networking.nameservers;
  # systemd-tmpfiles deletes X sockets that are not listening in its
  # /proc/net/unix, which is per-namespace, so it deletes guest's Xwayland
  # socket on every switch and clean, and games then fail to start.
  systemd.tmpfiles.rules = [ "x /tmp/.X11-unix/X*" ];
}
