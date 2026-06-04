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
        notify_on_cmd_finish = "unfocused 5 notify-bell";
        confirm_os_window_close = 0;
        # This seems to be broken on nixos, causing kitty to try to watch the
        # full nix store.
        #
        # TODO: Will be fixed in version > 0.47.1.
        auto_reload_config = -1;
      };
    };
  };
}
