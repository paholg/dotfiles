{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.vanta-agent;
in
{
  options = {
    services.vanta-agent = {
      enable = lib.mkEnableOption "Vanta Agent";
      package = lib.mkPackageOption pkgs "vanta-agent" { };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    systemd.packages = [ cfg.package ];

    # vanta-cli expects to find and use 'cert.pem' and 'osquery.em' in /var/vanta
    # this creates the service's StateDirectory in advance, and places the cert symlink there
    systemd.tmpfiles.rules = [
      "d /var/lib/vanta"
      "L /var/lib/vanta/cert.pem - - - - ${cfg.package}/var/vanta/cert.pem"
      "L /var/vanta/ - - - - /var/lib/vanta"
    ];

    systemd.services.vanta = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        PrivateTmp = true;
        NoNewPrivileges = true;
        RestrictNamespaces = true;
        RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6 AF_NETLINK";
        CapabilityBoundingSet = "CAP_DAC_OVERRIDE CAP_SYS_ADMIN CAP_WRITE CAP_CREATE CAP_FOWNER CAP_CHOWN";
        ProtectSystem = "strict";
        PrivateDevices = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHostname = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProtectHome = "read-only";
        SystemCallArchitectures = "native";
        SystemCallFilter = "@system-service";
        SystemCallErrorNumber = "EPERM";
        MemoryDenyWriteExecute = true;
        ProcSubset = "pid";
        LockPersonality = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        # vanta expects to find its many files in /var/vanta, but it also expects to be able to
        # write other files (and a log directory) in the same directory
        StateDirectory = "vanta";
        BindReadOnlyPaths =
          let
            # skip cert.pem because it shows up as an empty file if included here
            vantaFiles = [
              "launcher"
              "metalauncher"
              "osqueryd"
              "osquery-vanta.ext"
              "vanta-cli"
            ];
            fileToBind = file: "${cfg.package}/var/vanta/${file}:/var/vanta/${file}:norbind";
          in
          lib.map fileToBind vantaFiles;
      };
    };
  };
}
