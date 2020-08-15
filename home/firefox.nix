{ config, pkgs, lib, ... }:

let
  nur-no-pkgs = import (builtins.fetchTarball
    "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
in {
  programs.firefox = {
    enable = true;
    extensions = with nur-no-pkgs.repos.rycee.firefox-addons; [
      # augmented-steam
      # tree-style-tab
      bitwarden
      cookie-autodelete
      decentraleyes
      greasemonkey
      https-everywhere
      link-cleaner
      privacy-badger
      reddit-enhancement-suite
      ublock-origin
    ];

    profiles = {
      default = {
        isDefault = true;
        settings = {
          "browser.display.background_color" = "#bdbdbd";
          "browser.search.suggest.enabled" = false;
          "browser.startup.page" = 3;
          "browser.tabs.closeWindowWithLastTab" = false;
          "browser.urlbar.placeholderName" = "DuckDuckGo";
          "devtools.theme" = "dark";
          "experiments.activeExperiment" = false;
          "experiments.enabled" = false;
          "experiments.supported" = false;
          "extensions.pocket.enabled" = false;
          "general.smoothScroll" = false;
          "layout.css.devPixelsPerPx" = "1";
          "network.IDN_show_punycode" = true;
          "network.allow-experiments" = false;
          "signon.rememberSignons" = false;
          "widget.content.gtk-theme-override" = "Adwaita:dark";
        };
      };
    };
  };
}
