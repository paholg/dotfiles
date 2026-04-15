{ pkgs, ... }:
{

  home.packages = [
    (pkgs.writeShellApplication {
      name = "cam";
      runtimeInputs = with pkgs; [ guvcview ];
      text = # bash
        ''
          guvcview -d /dev/video2 &
          pid=$!
          sleep 3 && kill $pid
        '';
    })
    (pkgs.writeShellApplication {
      name = "fix-permissions";
      runtimeInputs = with pkgs; [ fd ];
      text = # bash
        ''
          fd . -td -0 | xargs -0 chmod 755
          fd . -tf -0 | xargs -0 chmod 644
        '';
    })
    (pkgs.writeShellApplication {
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
    })
    (pkgs.writeShellApplication {
      name = "git-pr";
      runtimeInputs = with pkgs; [
        gh
        git
      ];
      text = # bash
        ''
          title=$(git log -1 --pretty=%s)
          body=$(git log -1 --pretty=%b)

          git push -f
          gh pr create --fill "$@" || gh pr edit --title "$title" --body "$body"
        '';
    })
    (pkgs.writeShellApplication {
      name = "git-pb";
      runtimeInputs = with pkgs; [ git ];
      text = # bash
        ''
          # Source: https://github.com/not-an-aardvark/git-delete-squashed

          git fetch

          worktree_branches=$(git worktree list --porcelain | sed -n 's/^branch refs\/heads\///p')
          branches=$(git for-each-ref refs/heads/ "--format=%(refname:short)")
          for branch in $branches; do
            [[ "$branch" == "main" ]] && continue

            in_worktree=false
            if echo "$worktree_branches" | grep -qx "$branch"; then
              in_worktree=true
            fi

            if git merge-base --is-ancestor "$branch" origin/main; then
              if $in_worktree; then
                echo "In worktree:      $branch"
              else
                git branch -D "$branch" > /dev/null
                echo "Deleted (empty):  $branch"
              fi
              continue
            fi

            mergeBase=$(git merge-base origin/main "$branch")
            # Want the symbols to be literal
            # shellcheck disable=SC1083,SC1001
            tree=$(git rev-parse "$branch"\^{tree})
            commit_tree=$(git commit-tree "$tree" -p "$mergeBase" -m _)
            cherry=$(git cherry origin/main "$commit_tree")
            if [[ "$cherry" == "-"* ]]; then
              if $in_worktree; then
                echo "In worktree:      $branch"
              else
                git branch -D "$branch" > /dev/null
                echo "Deleted (merged): $branch"
              fi
            else
              echo "Not merged:       $branch"
            fi
          done
        '';
    })
    (pkgs.writeShellApplication {
      name = "git-refresh";
      runtimeInputs = with pkgs; [ git ];
      text = # bash
        ''
          n=$(git stash list | wc -l)
          git add .
          git stash
          git fetch
          git rebase origin/main "$@"
          [ "$(git stash list | wc -l)" -gt "$n" ] && git stash pop
          git reset
        '';
    })
    (pkgs.writeShellApplication {
      name = "mkfile";
      text = # bash
        ''
          mkdir -p "$(dirname "$1")"
          touch "$1"
        '';
    })
    (pkgs.writeShellApplication {
      name = "niri-focus-urgent";
      text = # bash
        ''
          id="$(niri msg --json windows | jq -r '[.[] | select(.is_urgent)][0].id // empty')"
          [ -n "$id" ] && niri msg action focus-window --id "$id"
        '';
    })
    (pkgs.writeShellApplication {
      name = "screenshot-edit";
      runtimeInputs = with pkgs; [
        grim
        slurp
        satty
      ];
      text = # bash
        ''
          grim -t ppm -g "$(slurp -d)" - \
          | satty -f - --copy-command="wl-copy" --initial-tool="arrow" --actions-on-enter="save-to-clipboard,exit"
        '';
    })
    (pkgs.writeShellApplication {
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
    })
  ];
}
