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
          "cargo tree --depth 1 -e normal --prefix none | rg '^([^ ]+) v([^ ]+).*$' -r '-p $1@$2' | xargs cargo doc --open --no-deps"
        ];
        need_stdout = false;
        on_success = "back";
      };
    };
  };
}
