{ config, lib, ... }:
with lib;
let
  cfg = config.custom;
in
{
  options.custom.starship = {
    enable = mkEnableOption "Starship";
    host_color = mkOption { type = types.str; };
  };

  config = mkIf cfg.starship.enable {
    programs.starship = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        right_format = "$kubernetes$aws";
        git_branch.format = "[$symbol$branch]($style) ";
        status.disabled = false;
        aws = {
          disabled = false;
        };
        hostname = {
          format = "[$hostname]($style):";
          style = "bold ${cfg.starship.host_color}";
        };
        kubernetes = {
          disabled = true;
          format = "[$symbol$context( ($namespace))]($style)";
        };
        nix_shell = {
          unknown_msg = "unk";
        };
        shell = {
          disabled = false;
          fish_indicator = "";
          unknown_indicator = "unk";
        };
        shlvl = {
          disabled = false;
        };
        sudo = {
          disabled = false;
        };
        time = {
          disabled = false;
          format = "$time ";
        };
        username = {
          format = "[$user]($style)@";
          show_always = if (cfg.username == "paho") then false else true;
        };
      };
    };
  };
}
