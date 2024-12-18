{ pkgs, ... }:
{
  imports = [ ../../home ];
  home.stateVersion = "24.05";

  custom = {
    username = "paho";
    gui = true;
    linux = true;
    nixos = true;
    starship.host_color = "cyan";
    # xmonad.enable = true;
    i3.enable = true;
    i3.customConfig = ''
      exec_always slack
      exec_always firefox
    '';
    fish_extra_init =
      # fish
      ''
        set TTY (tty)
        [ "$TTY" = "/dev/tty1" ] && exec "startx"
      '';
  };

  home.shellAliases = {
    my = "mycli --socket /tmp/mysql.sock -uroot -D scholarly_development";
  };

  home.packages = with pkgs; [
    awscli2
    csvtool
    distrobox
    heroku
    iredis
    mycli
    pscale
  ];
}
