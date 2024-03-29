{...}: {
  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          follow = "keyboard";
          geometry = "800x3-0+24";
          indicate_hidden = true;
          shrink = true;
          horizontal_padding = 24;
          font = "Monospace 12";
          format = "%a: %s";
          alignment = "right";
          word_wrap = true;
          dmenu = "/usr/bin/env dmenu";
          browser = "/usr/bin/env firefox";
          mouse_left_click = "do_action";
          mouse_middle_click = "close_all";
          mouse_right_click = "close_current";
          show_indicators = true;
        };
      };
    };
  };

  xsession = {
    enable = true;

    initExtra = ''
      monitor_switch default &

      fixkb &

      # Set cursor
      xsetroot -cursor_name left_ptr &

      # startup programs
      background 150 &
      /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
      beyond-identity credentials &

      xset s 600
      xss-lock slock +resetsaver

      # Load resources
      xrdb -merge .Xresources &
    '';

    windowManager = {
      xmonad = {
        enable = true;
        config = ./xmonad.hs;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
        ];
      };
    };
  };
}
