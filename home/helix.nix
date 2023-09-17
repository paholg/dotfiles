{ ... }:

{
  # Configure ra-multiplex for persistent rust-analyzer goodness!
  home.file = {
    ".config/ra-multiplex/config.toml".text = ''
      instance_timeout = false
      gc_interval = 10
      listen = ["127.0.0.1", 27631]
      connect = ["127.0.0.1", 27631]
      log_filters = "info"
    '';
  };

  systemd.user.services.ra-multiplex = {
    Unit = {
      Description = "Rust Analyzer multiplex server";
      Documentation = [ "https://github.com/pr2502/ra-multiplex" ];
    };
    Install.WantedBy = [ "default.target" ];
    Service = {
      ExecStart = "/bin/bash -c 'PATH=/home/paho/.cargo/bin:/home/paho/.nix-profile/bin ra-multiplex-server'";
    };
  };

  programs.helix = {
    enable = true;
    languages = {
      language = [
        {
          name = "git-commit";
          rulers = [ 51 73 ];
        }
        {
          name = "nix";
          language-server.command = "rnix-lsp";
          auto-format = true;
        }
        {
          name = "python";
          auto-format = true;
          formatter = {
            command = "black";
            args = [ "-" "--quiet" "--line-length=79" ];
          };
        }
        {
          name = "rust";
          rulers = [ 81 101 ];
          # TODO: Switch to ra-multiplex once this issue is resolved:
          # https://github.com/helix-editor/helix/issues/2479
          # language-server.command = "ra-multiplex";
          config = {
            # files.watcher = "server";
          };
        }
        {
          name = "toml";
          auto-format = true;
        }
      ];
    };

    settings = {
      editor = {
        indent-guides.render = true;
        file-picker = { hidden = false; };
        lsp = {
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
          # Swap a and A.
          A = "append_mode";
          a = "insert_at_line_end";

          # Custom bindings
          A-g = ":lang gotmpl";
          A-r = ":lsp-restart";
          A-h = ":toggle-option lsp.display-inlay-hints";
        };
      };
    };
  };
}
