{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./guest-tv.nix
  ];
  system.stateVersion = "23.11";
  networking.hostName = "fractal";

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 65 * 1024;
    }
  ];

  # Need to install here for polit rules to be picked up.
  environment.systemPackages = [ pkgs.quickemu ];
  virtualisation.spiceUSBRedirection.enable = true;

  # HP EX920 (SM2262, fw SVN163) has a broken NVMe FLUSH: ~240ms/fsync.
  # Its volatile write cache is already disabled (WCE=0), so writes are
  # durable on completion and FLUSH is unnecessary — tell the kernel to stop
  # issuing it. ~2000x faster fsync, no durability loss, no throughput cost.
  services.udev.extraRules = ''
    ACTION=="add|change", SUBSYSTEM=="block", ATTRS{model}=="HP SSD EX920 1TB*", ATTR{queue/write_cache}="write through"
  '';

  # For display-switch
  hardware.i2c.enable = true;

  networking.networkmanager.enable = false;
  networking.wireless.enable = false;

  # Containers
  virtualisation.podman = {
    enable = false;
    dockerCompat = false;
  };
  virtualisation.docker.enable = true;
  users.users.paho.extraGroups = [ "docker" ];

  # Keep exec sessions live when switching.
  systemd.services.docker.restartIfChanged = false;

  # For rebinding mouse
  services.ratbagd.enable = true;

  # GC 11.0.0 firmware after 20260622 hangs Navi31 with "Illegal opcode in
  # command stream"; AMD reverted it 2026-09-18. Remove once nixpkgs ships a
  # release containing the revert.
  # https://github.com/HansKristian-Work/vkd3d-proton/issues/3004
  nixpkgs.overlays = [
    (final: prev: {
      linux-firmware = prev.linux-firmware.overrideAttrs (old: rec {
        version = "20260622";
        src = prev.fetchFromGitLab {
          owner = "kernel-firmware";
          repo = "linux-firmware";
          tag = version;
          hash = "sha256-nSoJhgI4hAxtNmnj5M6ticzuBSt9uNAYcmc1VR/yXxE=";
        };
      });
    })
  ];

  # Might be causing amdgpu page faults?
  # services.lact.enable = true;
  # hardware.amdgpu.overdrive.enable = true;

  programs.gamemode.enable = true;

  programs.steam = {
    enable = true;
    package = pkgs.steam.override {
      # Hack to fix Proton games using the wrong timezone.
      # https://github.com/NixOS/nixpkgs/issues/338266#issuecomment-2419568331
      extraBwrapArgs = [ "--unsetenv TZ" ];
    };
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;

    protontricks.enable = true;
  };
}
