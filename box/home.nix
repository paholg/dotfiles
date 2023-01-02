{ pkgs, ... }:

{
  imports = [ ../home/common.nix ../home/common-linux.nix ];

  home = {
    username = "paho";
    homeDirectory = "/home/paho";

    sessionVariables = {
      IMDB_RENAME_DATA_DIR = "/mnt/storage/imdb-rename";
      IMDB_RENAME_DEST_DIR = "/mnt/storage/";
      IMDB_RENAME_TEMPLATE =
        "{{ if episode }} tv/{series.primaryTitle} ({title.startYear})/Season {episode.seasonNumber | leading_zero}/{series.primaryTitle} - s{episode.seasonNumber | leading_zero}e{episode.episodeNumber | leading_zero} - {title.primaryTitle} {{ else }} {title.primaryTitle} ({title.startYear})";
    };
  };

  home.file = {
    ".config/tvnamer/tvnamer.json".text = (builtins.readFile ./tvnamer.json);
  };

  programs.git.userEmail = "paho@paholg.com";
}
