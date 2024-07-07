{ config, lib, ... }:
with lib;
let
  cfg = config.custom;
in
{
  options.custom.ssh = {
    enable = mkEnableOption "Ssh server";
  };

  config = mkIf cfg.ssh.enable {
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
      # ubuntu
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBIh01cLJMxFWbyny+uH/nM2j0Mkl3Gar95/6/08fb+J+aFlYnT2Wu6zthZyQ00kblmszIwEgtOOJEfyJOCaPLrs= paho@ubuntu"
      # fractal
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINiwxr54qPq+/0gJNA0QGJRWh1VVhTGraYirDQ30wcDI paho@nixos"
      # box
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINuKvOi7aZTRK6fSxYWJ59t+ep2SUqumNn+J4p5wBzUH paho@box"
    ];
  };
}
