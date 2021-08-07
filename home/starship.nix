{ ... }:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      git_branch.format = "[$symbol$branch]($style) ";
      hostname.format = "[$hostname]($style):";
      status.disabled = false;
      time = {
        disabled = false;
        format = "$time ";
      };
      username.format = "[$user]($style)@";
    };
  };
}
