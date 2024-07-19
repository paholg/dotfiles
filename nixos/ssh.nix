{ config, lib, ... }:
let
  cfg = config.custom;
in
{
  config = lib.mkIf cfg.ssh {
    services.openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
      };
    };

    networking.firewall.allowedUDPPortRanges = [
      # Ports for Mosh
      {
        from = 60001;
        to = 60999;
      }
    ];

    users.users.paho.openssh.authorizedKeys.keys = [
      # phone
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAz7dvsIeGWRD3zTaenldrKwPJ0z+9fGuDOHkOa4luJd JuiceSSH"
      # fractal
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINiwxr54qPq+/0gJNA0QGJRWh1VVhTGraYirDQ30wcDI paho@nixos"
      # box
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINuKvOi7aZTRK6fSxYWJ59t+ep2SUqumNn+J4p5wBzUH paho@box"
      # t14s
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIQJXbrxsf62WbCtljpQBzonh0SZ7A0g9SsqKbdy0amE paho@t14s"
    ];
  };
}
