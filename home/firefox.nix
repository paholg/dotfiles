{ config, lib, ... }:
let
  cfg = config.custom;

  extra_settings =
    if (cfg.username == "paho") then
      {
        "services.sync.username" = "paho@paholg.com";

        # These settings together disable the default tab bar:
        # "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        # "browser.tabs.tabmanager.enabled" = false;
      }
    else
      { };
in
{
  config = lib.mkIf cfg.gui {
    programs.firefox = {
      enable = true;

      profiles.default = {
        isDefault = true;
        settings = {
          "browser.display.background_color" = "#bdbdbd";
          "browser.download.dir" = "/home/${cfg.username}/downloads";
          "browser.search.suggest.enabled" = false;
          "browser.startup.page" = 3;
          "browser.urlbar.placeholderName" = "Kagi";
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
        } // extra_settings;
      };
    };

    home.file.".mozilla/firefox/default/chrome/userChrome.css".text = # css
      ''
        #TabsToolbar {
          visibility: collapse !important;
        }
      '';
  };
}
