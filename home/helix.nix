{
  lib,
  pkgs,
  ...
}:
# ra_multiplex = "${lib.getExe' pkgs.ra-multiplex "ra-multiplex"}";
{
  config = {
    home.file.".config/helix/themes/paho-theme.toml".source = ./helix-theme.toml;

    # Configure ra-multiplex for persistent rust-analyzer goodness!
    # home.file = {
    #   ".config/ra-multiplex/config.toml".source = (pkgs.formats.toml {}).generate "" {
    #     instance_timeout = false;
    #     min_available_memory = "4 GiB";
    #     gc_interval = 10;
    #     listen = ["127.0.0.1" 27631];
    #     connect = ["127.0.0.1" 27631];
    #     log_filters = "info";
    #   };
    # };

    # systemd.user.services.ra-multiplex = {
    #   Unit = {
    #     Description = "Rust Analyzer multiplex server";
    #     Documentation = ["https://github.com/pr2502/ra-multiplex"];
    #   };
    #   Install.WantedBy = ["default.target"];
    #   Service = {
    #     ExecStart = "/bin/bash -lc 'CARGO_TARGET_DIR=/home/paho/.cargo/cache2 ${ra_multiplex} server'";
    #   };
    # };

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
            # formatter = {
            #   command = "black";
            #   args = ["-" "--quiet" "--line-length=79"];
            # };
          }
          {
            name = "rust";
            rulers = [
              81
              101
            ];
            # language-servers = ["ra-multiplex"];
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
          # ra-multiplex = {
          #   command = ra_multiplex;
          #   args = ["client"];
          # };
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
          normal = {
            space = {
              f = "file_picker_in_current_directory";
              F = "file_picker";
              c = "file_picker_in_current_buffer_directory";
              C = "file_browser";
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
