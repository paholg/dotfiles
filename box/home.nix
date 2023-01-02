{ pkgs, ... }:

{
  imports = [ ../home/common.nix ../home/common-linux.nix ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";

    sessionVariables = {
      IMDB_RENAME_DATA_DIR = "/mnt/storage/imdb-rename";
      IMDB_RENAME_DEST_DIR = "/mnt/storage/";
    };
  };

  home.file = {
    ".config/tvnamer/tvnamer.json".text = (builtins.readFile ./tvnamer.json);
  };

  programs.git.userEmail = "paho@paholg.com";

  home.packages = with pkgs; [ tvnamer ];
}
