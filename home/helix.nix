{
  lib,
  pkgs,
  ...
}:
{
  config = {
    home.file.".config/helix/themes/paho-theme.toml".source = ./helix-theme.toml;

    programs.helix = {
      enable = true;
      defaultEditor = true;
      package = pkgs.external.helix;

      settings = {
        editor = {
          color-modes = true;
          completion-replace = true;
          end-of-line-diagnostics = "hint";
          file-picker.hidden = false;
          indent-guides.render = true;
          lsp.auto-signature-help = false;
          lsp.display-inlay-hints = true;
          rulers = [ 81 ];
          soft-wrap.enable = true;
          # Flickery and hides the top row when multiple panes open :(
          # sticky-context = {
          #   enable = true;
          #   indicator = false;
          # };
          true-color = true;
        };

        theme = "paho-theme";

        keys = {
          insert = {
            C-h = "goto_prev_tabstop";
            C-l = "goto_next_tabstop";
          };
          normal = {
            X = "extend_line_above";
            space = {
              c = "file_picker_in_current_buffer_directory";
              l = [
                ":new"
                ":insert-output lazygit"
                ":buffer-close!"
                ":redraw"
              ];
            };
            A-h = ":toggle-option lsp.display-inlay-hints";
          };
        };
      };

      languages = {
        language = [
          {
            name = "c";
            file-types = [
              "c"
              "keymap"
            ];
          }
          {
            name = "git-commit";
            rulers = [
              51
              73
            ];
          }
          {
            name = "json";
            auto-format = false;
          }
          {
            name = "markdown";
            comment-tokens = [
              "-"
              "+"
              "*"
              "1."
              ">"
              "- [ ]"
            ];
          }
          {
            name = "nickel";
            auto-format = true;
          }
          {
            name = "nix";
            auto-format = true;
            language-servers = [ "nixd" ];
            formatter = {
              command = lib.getExe pkgs.nixfmt-rfc-style;
              args = [ ];
            };
          }
          {
            name = "python";
            auto-format = true;
          }
          {
            name = "rust";
            rulers = [
              81
              101
            ];
          }
          {
            name = "ruby";
            language-servers = [ "ruby-lsp" ];
          }
          {
            name = "toml";
            auto-format = true;
          }
        ];

        language-server = {
          nixd = {
            command = lib.getExe pkgs.nixd;
          };
          ruby-lsp = {
            command = lib.getExe pkgs.ruby-lsp;
          };
        };
      };
    };
  };
}
