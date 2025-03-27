{ ... }:
{
  config = {
    programs.alacritty = {
      enable = true;
      settings = {
        scrolling = {
          history = 100000;
          multiplier = 3;
        };

        # hints = {
        #   enabled = [
        #     # Default Hint handler, with file links removed.
        #     {
        #       command = "xdg-open";
        #       hyperlinks = true;
        #       post_processing = true;
        #       persist = false;
        #       mouse.enabled = true;
        #       binding = {
        #         key = "U";
        #         mods = "Control|Shift";
        #       };
        #       regex = ''(ipfs:|ipns:|magnet:|mailto:|gemini://|gopher://|https://|http://|news:|file:|git://|ssh:|ftp://)[^\\u{0000}-\\u{001F}\\u{007F}-\\u{009F}<>\\"\\\\s{-}\\\\^⟨⟩`]+'';
        #     }
        #     # Copy file links.
        #     {
        #       action = "Copy";
        #       hyperlinks = true;
        #       post_processing = true;
        #       persist = false;
        #       mouse.enabled = true;
        #       binding = {
        #         key = "F";
        #         mods = "Control|Shift";
        #       };
        #       regex = ''(file:)[^\\u{0000}-\\u{001F}\\u{007F}-\\u{009F}<>\\"\\\\s{-}\\\\^⟨⟩`]+'';
        #     }
        #   ];
        # };

        window.dynamic_title = true;
        window.decorations = "None";
      };
    };
  };
}
