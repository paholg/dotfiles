{ ... }:
{
  config = {
    programs.alacritty = {
      enable = true;
      settings = {
        scrolling = {
          history = 100000;
          multiplier = 3;
        };

        window.dynamic_title = true;
        window.decorations = "None";
      };
    };
  };
}
