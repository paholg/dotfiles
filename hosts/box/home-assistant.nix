{ config, pkgs, ... }:
{
  services.mosquitto = {
    enable = true;
    dataDir = config.custom.drives.storage + "/mosquitto";
    listeners = [
      {
        acl = [ "pattern readwrite #" ];
        omitPasswordAuth = true;
        settings.allow_anonymous = true;
        address = "127.0.0.1";
        port = config.custom.ports.mqtt;
      }
    ];
  };

  users.users.zigbee2mqtt.extraGroups = [ "dialout" ];

  services.zigbee2mqtt = {
    enable = true;
    dataDir = config.custom.drives.storage + "/zigbee2mqtt";
    settings = {
      mqtt = {
        base_topic = "zigbee2mqtt";
        server = "mqtt://127.0.0.1:${toString config.custom.ports.mqtt}";
      };
      serial = {
        adapter = "ember";
        port = "/dev/serial/by-id/usb-Nabu_Casa_ZBT-2_DCB4D90C0A64-if00";
        # See https://www.zigbee2mqtt.io/guide/adapters/emberznet.html
        baudrate = 460800;
        rtscts = true;
      };
      frontend = {
        enabled = true;
        port = config.custom.ports.zigbee_frontend;
        base_url = "/zigbee";
      };
      advanced.log_level = "debug";
    };
  };

  services.home-assistant = {
    enable = true;
    configDir = config.custom.drives.storage + "/home-assistant";
    extraComponents = [
      "esphome"
      "local_calendar"
      "met" # Forecast
      "mqtt" # For Zigbee
      "radio_browser"
      "roborock"
      "timer"
    ];
    customComponents = [
      pkgs.home-assistant-custom-components.auth_oidc
    ];
    customLovelaceModules = with pkgs.home-assistant-custom-lovelace-modules; [
      auto-entities
      card-mod
    ];
    lovelaceConfig = { };

    config = {
      default_config = { };
      scene = "!include scenes.yaml";
      automation = "!include automations.yaml";
      script = "!include scripts.yaml";
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" ];
      };
      auth_oidc = {
        client_id = "home-assistant";
        discovery_url = "https://auth.paholg.com/oauth2/openid/home-assistant/.well-known/openid-configuration";
        display_name = "Login with Kanidm";
        claims = {
          username = "preferred_username";
          display_name = "name";
          groups = "groups";
        };
        roles = {
          admin = "home_assistant_admin@auth.paholg.com";
          user = "home_assistant_user@auth.paholg.com";
        };
        id_token_signing_alg = "ES256";
        features = {
          automatic_person_creation = true;
        };
      };
    };
  };

  services.nginx.virtualHosts."ha.paholg.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.custom.ports.home_assistant}";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
    };
  };
}
