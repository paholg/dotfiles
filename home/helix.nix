{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.custom.helix;
in
# ra_multiplex = "${lib.getExe' pkgs.ra-multiplex "ra-multiplex"}";
{
  options.custom.helix = {
    enable = mkEnableOption "Helix";
  };
  config = mkIf cfg.enable {
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
      package = pkgs.helix;
      languages = {
        language = [
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
            name = "toml";
            auto-format = true;
          }
        ];

        language-server = {
          nil = {
            config.nil.formatting.command = [ "${lib.getExe pkgs.nixfmt-rfc-style}" ];
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
          auto-pairs = false;
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
        };
        theme = "paho-theme";

        keys = {
          normal = {
            space = {
              f = "file_picker_in_current_directory";
              F = "file_picker";
              c = "file_picker_in_current_buffer_directory";
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
