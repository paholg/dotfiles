{
  ...
}:
{
  config = {
    programs.zed-editor = {
      enable = true;

      extensions = [
        "git-firefly"
        "nix"
        "ruby"
        "sorbet"
        "sql"
        "toml"
        "typst"
      ];

      mutableUserSettings = false;

      userSettings = {
        auto_update = false;
        autosave.after_delay.milliseconds = 100;
        buffer_font_family = "Monaspace Neon";
        calls.mute_on_join = true;
        cursor_blink = false;
        cursor_shape = "block";
        git.inline_blame.enabled = false;
        helix_mode = true;
        soft_wrap = "editor_width";
        telemetry.metrics = false;
        theme = "XY Zed";
        ui_font_family = "Monaspace Neon";
        which_key.enabled = true;

        agent = {
          default_model = {
            provider = "anthropic";
            model = "claude-opus-4-5-latest";
          };
        };
        buffer_font_features = {
          calt = true;
          liga = true;
          ss01 = true;
          ss02 = true;
          ss03 = true;
          ss04 = true;
          ss05 = true;
          ss07 = true;
          ss09 = true;
          ss10 = true;
        };
        gutter = {
          folds = false;
          runnables = false;
          breakpoints = false;
          min_line_number_digits = 3;
        };
      };

      # userKeymaps = [ ];
    };
  };
}
