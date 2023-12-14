{...}: {
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    enableFishIntegration = true;
    settings = {
      right_format = "$kubernetes$aws";
      git_branch.format = "[$symbol$branch]($style) ";
      hostname.format = "[$hostname]($style):";
      status.disabled = false;
      aws = {disabled = false;};
      kubernetes = {
        disabled = true;
        format = "[$symbol$context( ($namespace))]($style)";
      };
      nix_shell = {
        heuristic = true;
        unknown_msg = "unknown";
      };
      time = {
        disabled = false;
        format = "$time ";
      };
      username.format = "[$user]($style)@";
    };
  };
}
