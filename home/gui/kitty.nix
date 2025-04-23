{ ... }:
{
  config = {
    programs.kitty = {
      enable = true;
      shellIntegration.mode = "no-cursor";
      settings = {
        cursor_shape = "block";
        cursor_blink_interval = 0;
        scrollback_lines = 100000;
        enable_audio_bell = false;
        visual_bell_duration = 0.1;
        hide_window_decorations = "yes";
      };
    };
  };
}
