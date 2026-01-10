This repo contains the NixOS and HomeManager configuration for several hosts.

* When trying to diagnose issues, find proof on an underlying cause from systemd
  logs and the like before proposing a solution.
* When proposing configuration changes, first find documentation for those
  configuration options and provide references to them.
* Often, diagnostic commands will require permissions that you lack, such as
  `sudo` or permissions for subdirectories of `/mnt/storage`. Instead of
  attempting these commands, ask me to run them.
