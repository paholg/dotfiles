{ config, lib, ... }:
{
  options.custom.starship = {
    host_color = lib.mkOption { type = lib.types.str; };
  };

  config = {
    programs.starship = {
      enable = true;
      enableFishIntegration = true;
      settings = {
        format = "$username$hostname$localip$directory$custom$all$shlvl$character";
        git_branch.format = "[$symbol$branch]($style) ";
        status.disabled = false;
        aws = {
          disabled = false;
          format = ''[$symbol($profile )(\($region\) )(\[$duration\] )]($style)'';
          region_aliases = {
            us-east-1 = "use1";
            us-east-2 = "use2";
            us-west-1 = "usw1";
            us-west-2 = "usw2";
          };
        };
        custom = {
          heroku = {
            command = "echo $HEROKU_APP";
            when = ''[ -n "$HEROKU_APP" ]'';
            format = "[$symbol $output]($style) ";
            style = "#D7BFF2 italic";
            symbol = "";
            shell = "bash";
          };
        };
        # custom = {
        #   jj = {
        #     when = true;
        #     description = "current jj status";
        #     symbol = "";
        #     command = # bash
        #       ''
        #         jj root > /dev/null && jj log --revisions @ --no-graph --ignore-working-copy --color always --limit 1 --template '
        #           separate(" ",
        #             "🥋",
        #             change_id.shortest(4),
        #             bookmarks,
        #             "|",
        #             concat(
        #               if(conflict, "💥"),
        #               if(divergent, "🚧"),
        #               if(hidden, "👻"),
        #               if(immutable, "🔒"),
        #             ),
        #             raw_escape_sequence("\x1b[0;32m") ++ if(empty, "(empty)"),
        #             raw_escape_sequence("\x1b[0;32m") ++ if(description.first_line().len() == 0,
        #               "(no desc)",
        #               if(description.first_line().substr(0, 20) == description.first_line(),
        #                 description.first_line(),
        #                 description.first_line().substr(0, 20) ++ "…",
        #               )
        #             ) ++ raw_escape_sequence("\x1b[0m"),
        #           )
        #         '
        #       '';
        #   };
        #   git_branch = {
        #     when = true;
        #     command = "jj root > /dev/null 2>&1 || starship module git_branch";
        #     description = "Re-enable git when not in a jj repo";
        #   };
        #   git_commit = {
        #     when = true;
        #     command = "jj root > /dev/null 2>&1 || starship module git_commit";
        #     description = "Re-enable git when not in a jj repo";
        #   };
        #   git_state = {
        #     when = true;
        #     command = "jj root > /dev/null 2>&1 || starship module git_state";
        #     description = "Re-enable git when not in a jj repo";
        #   };
        #   git_status = {
        #     when = true;
        #     command = "jj root > /dev/null 2>&1 || starship module git_status";
        #     description = "Re-enable git when not in a jj repo";
        #   };
        # };
        hostname = {
          format = "[$hostname]($style):";
          style = "bold ${config.custom.starship.host_color}";
        };
        kubernetes = {
          disabled = false;
          format = "[$symbol$context( ($namespace))]($style)";
        };
        nix_shell = {
          unknown_msg = "unk";
          # heuristic = true;
        };
        shell = {
          disabled = false;
          fish_indicator = "";
          unknown_indicator = "unk";
        };
        shlvl = {
          disabled = false;
          format = "[$symbol]($style)";
          repeat = true;
          symbol = "❯";
          repeat_offset = 2;
          style = "cyan";
          threshold = 0;
        };
        sudo = {
          disabled = false;
        };
        time = {
          disabled = false;
          format = "$time ";
        };
        username = {
          format = "[$user]($style)@";
          show_always = if (config.custom.username == "paho") then false else true;
        };
      };
    };
  };
}
