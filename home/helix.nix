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
            command = "${lib.getExe pkgs.nixd}";
          };
          ruby-lsp = {
            command = "${lib.getExe pkgs.ruby-lsp}";
          };
        };
      };

      settings = {
        editor = {
          true-color = true;
          bufferline = "multiple";
          indent-guides.render = true;
          file-picker = {
            hidden = false;
          };
          lsp = {
            auto-signature-help = false;
            display-messages = true;
            display-inlay-hints = true;
          };
          rulers = [ 81 ];
          soft-wrap.enable = true;
          whitespace.render = "none";
          end-of-line-diagnostics = "hint";
        };
        theme = "paho-theme";

        keys = {
          insert = {
            C-h = "goto_prev_tabstop";
            C-l = "goto_next_tabstop";
          };
          normal = {
            space = {
              f = "file_picker_in_current_directory";
              F = "file_picker";
              c = "file_picker_in_current_buffer_directory";
              l = [
                ":new"
                ":insert-output lazygit"
                ":buffer-close!"
                ":redraw"
              ];
            };

            # Custom bindings
            # A-r = ":lsp-custom rust-analyzer/reloadWorkspace";
            A-h = ":toggle-option lsp.display-inlay-hints";
          };
        };
      };
    };
  };
}
