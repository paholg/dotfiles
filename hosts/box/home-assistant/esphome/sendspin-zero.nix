# Builds a SendspinZero speaker:
# https://github.com/RealDeco/SendspinZero
#
# Adding a device to Home Assistant isn't declarative: add it in the ESPHome
# integration with its API key.
{ lib, pkgs }:
let
  npins = import ../../../../npins;
  yaml = pkgs.formats.yaml { tags = true; };
  secret = var: { "!env_var" = var; };
  remove = { "!remove" = null; };

in
{
  name,
  friendlyName,
  # The upstream config to use: Sendspin_Zero-<variant>.yaml.
  variant,
  # Reserved in DHCP; used for OTA updates and logs.
  address,
  ...
}:
let
  # Removes remote packages, which have no pinned ref.
  upstream = pkgs.runCommand "sendspin-zero-${lib.toLower variant}.yaml" { } ''
    sed '/^packages:/,/^      - devices\//d' \
      ${npins.sendspin-zero}/Sendspin_Zero-${variant}.yaml > $out
  '';

  device = {
    packages.upstream."!include" = "${upstream}";

    substitutions = {
      inherit name;
      friendly_name = friendlyName;
      ha_weather_entity = (import ../entities.nix { inherit lib; }).weather.home.entity_id;
    };

    esphome.name_add_mac_suffix = false;

    # Read from the environment, which `just esphome` loads from the `esphome_env` secret.
    api.encryption.key = secret "API_KEY";
    ota = [
      {
        platform = "esphome";
        password = secret "OTA_PASSWORD";
      }
    ];
    wifi = {
      ssid = secret "WIFI_SSID";
      password = secret "WIFI_PASSWORD";
      power_save_mode = "none";
      use_address = address;
      # The credentials are compiled in, so the open fallback hotspot isn't needed.
      ap = remove;
    };
    captive_portal = remove;
    improv_serial = remove;
  };
in
{
  # ESPHome writes a .gitignore beside the config unless one exists.
  custom.esphome.${name} = pkgs.linkFarm "esphome-${name}" {
    "${name}.yaml" = yaml.generate "${name}.yaml" device;
    ".gitignore" = pkgs.emptyFile;
  };
}
