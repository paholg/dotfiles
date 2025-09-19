{ pkgs, ... }:
let
  cam = pkgs.writeShellApplication {
    name = "cam";
    runtimeInputs = with pkgs; [ guvcview ];
    text = # bash
      ''
        guvcview -d /dev/video2 &
        pid=$!
        sleep 3 && kill $pid
      '';
  };
  fix-permissions = pkgs.writeShellApplication {
    name = "fix-permissions";
    runtimeInputs = with pkgs; [ fd ];
    text = # bash
      ''
        fd . -td -0 | xargs -0 chmod 755
        fd . -tf -0 | xargs -0 chmod 644
      '';
  };
  gh-update = pkgs.writeShellApplication {
    name = "gh-update";
    runtimeInputs = with pkgs; [
      gh
      git
    ];
    text = # bash
      ''
        branch=$(git branch --show-current)
        pr=$(gh pr list --head "$branch" --json number -q '.[0].number')
        title=$(git log -1 --pretty=%s)
        body=$(git log -1 --pretty=%b)

        echo "Updating PR #$pr title and body"

        gh pr edit "$pr" --title "$title" --body "$body"
      '';
  };
  git-clean-merged = pkgs.writeShellApplication {
    name = "git-clean-merged";
    runtimeInputs = with pkgs; [
      git
    ];
    text = # bash
      ''
        branch=$(git branch --show-current)
        pr=$(gh pr list --head "$branch" --json number -q '.[0].number')
        title=$(git log -1 --pretty=%s)
        body=$(git log -1 --pretty=%b)

        echo "Updating PR #$pr title and body"

        gh pr edit "$pr" --title "$title" --body "$body"
      '';
  };
  mkfile = pkgs.writeShellApplication {
    name = "mkfile";
    text = # bash
      ''
        mkdir -p "$(dirname "$1")"
        touch "$1"
      '';
  };
  play = pkgs.writeShellApplication {
    name = "play";
    text = # bash
      ''

        # A modified version of the playground script found here:
        # https://www.greyblake.com/blog/rust-playground-at-your-fingertips/
        #
        # While the original runs vim and cargo in tmux panes, this one opens your
        # $EDITOR in a separate kitty terminal.
        #
        # Depends on `kitty` and `bacon`.

        PLAYGROUND_DIR="/tmp/rust_playground"
        TIMESTAMP=$(date +"%Y-%m-%d-%H%M%S")
        PROJECT_DIR="$PLAYGROUND_DIR/pg-$TIMESTAMP"

        cargo new "$PROJECT_DIR"
        cd "$PROJECT_DIR"

        if (( $# > 0 )); then
            cargo add "$@"
        fi

        kitty -e "$EDITOR" "./src/main.rs" 2> /dev/null &
        bacon -j run

        printf "\n\nTo continue working on this project:\ncd %s\n" "$PROJECT_DIR"
      '';
  };
in
{

  home.packages = [
    cam
    fix-permissions
    gh-update
    git-clean-merged
    mkfile
    play
  ];
}
