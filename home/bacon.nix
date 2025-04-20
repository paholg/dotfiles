{ ... }:
{
  programs.bacon = {
    enable = true;
    settings = {
      jobs.doc-open = {
        command = [
          "/usr/bin/env"
          "bash"
          "-euo"
          "pipefail"
          "-c"
          "cargo tree --depth 1 -e normal --prefix none | cut -d' ' -f1 | xargs printf -- '-p %s\n' | xargs cargo doc --open --no-deps"
        ];
        need_stdout = false;
        on_success = "back";
      };
    };
  };
}
