{config, ...}: let
  nur = config.nur;
in {
  programs.firefox = {
    enable = true;

    profiles.default = {
      isDefault = true;
      settings = {
        "browser.display.background_color" = "#bdbdbd";
        "browser.download.dir" = "$HOME/downloads";
        "browser.search.suggest.enabled" = false;
        "browser.startup.page" = 3;
        "browser.urlbar.placeholderName" = "DuckDuckGo";
        "devtools.theme" = "dark";
        "experiments.activeExperiment" = false;
        "experiments.enabled" = false;
        "experiments.supported" = false;
        "extensions.pocket.enabled" = false;
        "general.smoothScroll" = false;
        "layout.css.devPixelsPerPx" = "1";
        "media.videocontrols.picture-in-picture.enabled" = false;
        "network.IDN_show_punycode" = true;
        "network.allow-experiments" = false;
        "signon.rememberSignons" = false;
        "widget.content.gtk-theme-override" = "Adwaita:dark";
      };
      extensions = with nur.repos.rycee.firefox-addons; [
        augmented-steam
        tree-style-tab
        bitwarden
        cookie-autodelete
        decentraleyes
        greasemonkey
        link-cleaner
        privacy-badger
        reddit-enhancement-suite
        ublock-origin
      ];
    };
  };
}
