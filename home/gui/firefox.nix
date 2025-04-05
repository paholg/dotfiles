{ config, ... }:
let
  extra_settings =
    if (config.custom.username == "paho") then
      {
        "services.sync.username" = "paho@paholg.com";
      }
    else
      { };
in
{
  config = {
    programs.firefox = {
      enable = true;

      profiles.default = {
        isDefault = true;
        settings = {
          "browser.display.background_color" = "#bdbdbd";
          "browser.download.dir" = "/home/${config.custom.username}/downloads";
          "browser.search.suggest.enabled" = false;
          "browser.startup.page" = 3;
          "browser.urlbar.placeholderName" = "Kagi";
          "devtools.theme" = "dark";
          "extensions.pocket.enabled" = false;
          "general.smoothScroll" = false;
          "media.videocontrols.picture-in-picture.enabled" = false;
          "signon.rememberSignons" = false;
          "widget.content.gtk-theme-override" = "Adwaita:dark";
        } // extra_settings;
      };
    };
  };
}
