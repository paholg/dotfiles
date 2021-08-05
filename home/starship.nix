{ ... }:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      hostname.ssh_only = true;
      status.disabled = false;
      time = {
        disabled = false;
        format = "$time ";
      };
    };
  };
}
