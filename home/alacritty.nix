{ config, lib, ... }:
with lib;
let
  cfg = config.custom.alacritty;
in
{
  options.custom.alacritty = {
    enable = mkEnableOption "alacritty";
  };

  config = mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        keyboard.bindings = [
          {
            key = "N";
            mods = "Control";
            action = "CreateNewWindow";
          }
        ];

        shell = {
          program = "fish";
          args = [ "--login" ];
        };

        scrolling = {
          history = 100000;
          multiplier = 3;
        };

        window.dynamic_title = true;

        font = {
          size = 13.0;
          normal = {
            family = "FiraCode Nerd Font Mono";
            style = "Regular";
          };
          bold = {
            family = "FiraCode Nerd Font Mono";
            style = "Bold";
          };
          italic = {
            family = "FiraCode Nerd Font Mono";
            style = "Italic";
          };
          bold_italic = {
            family = "FiraCode Nerd Font Mono";
            style = "Bold Italic";
          };
        };

        # From:
        # https://github.com/aaron-williamson/base16-alacritty/blob/master/colors/base16-spacemacs-256.yml
        # Base16 Spacemacs 256 - alacritty color config Nasser Alshammari
        # (https://github.com/nashamri/spacemacs-theme)
        colors = {
          primary = {
            background = "0x1f2022";
            foreground = "0xa3a3a3";
          };

          cursor = {
            text = "0x1f2022";
            cursor = "0xa3a3a3";
          };

          normal = {
            black = "0x1f2022";
            red = "0xf2241f";
            green = "0x67b11d";
            yellow = "0xb1951d";
            blue = "0x4f97d7";
            magenta = "0xa31db1";
            cyan = "0x2d9574";
            white = "0xa3a3a3";
          };

          bright = {
            black = "0x585858";
            red = "0xf2241f";
            green = "0x67b11d";
            yellow = "0xb1951d";
            blue = "0x4f97d7";
            magenta = "0xa31db1";
            cyan = "0x2d9574";
            white = "0xf8f8f8";
          };

          indexed_colors = [
            {
              index = 16;
              color = "0xffa500";
            }
            {
              index = 17;
              color = "0xb03060";
            }
            {
              index = 18;
              color = "0x282828";
            }
            {
              index = 19;
              color = "0x444155";
            }
            {
              index = 20;
              color = "0xb8b8b8";
            }
            {
              index = 21;
              color = "0xe8e8e8";
            }
          ];
        };
      };
    };
  };
}
