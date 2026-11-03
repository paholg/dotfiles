# Builds a SendspinZero speaker without a display, an ESP32-S3 with a PCM5102A
# DAC: https://github.com/RealDeco/SendspinZero
#
# Adding a device to Home Assistant isn't declarative: add it in the ESPHome
# integration with its API key.
{ pkgs }:
let
  yaml = pkgs.formats.yaml { tags = true; };
  secret = var: { "!env_var" = var; };
in
{
  name,
  friendlyName,
  # Reserved in DHCP; used for OTA updates and logs.
  address,
  ...
}:
let
  device = {
    esphome = {
      inherit name;
      friendly_name = friendlyName;
    };

    esp32 = {
      variant = "esp32s3";
      framework.type = "esp-idf";
    };

    psram = {
      mode = "quad";
      speed = "80MHz";
    };

    logger.level = "INFO";

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
    };

    # Unpaired access is on by default, so Music Assistant connects without pairing.
    sendspin = { };

    i2s_audio = [
      {
        id = "i2s_bus";
        i2s_lrclk_pin = "GPIO4";
        i2s_bclk_pin = "GPIO5";
      }
    ];

    speaker = [
      {
        platform = "i2s_audio";
        id = "dac";
        i2s_audio_id = "i2s_bus";
        i2s_dout_pin = "GPIO6";
        dac_type = "external";
        channel = "stereo";
      }
    ];

    media_source = [
      {
        platform = "sendspin";
        id = "sendspin_source";
      }
    ];

    media_player = [
      {
        platform = "speaker_source";
        id = "player";
        name = "Media Player";
        media_pipeline = {
          # Defaults to 16000.
          sample_rate = 48000;
          speaker = "dac";
          sources = [ "sendspin_source" ];
        };
      }
    ];
  };
in
{
  # ESPHome writes a .gitignore beside the config unless one exists.
  custom.esphome.${name} = pkgs.linkFarm "esphome-${name}" {
    "${name}.yaml" = yaml.generate "${name}.yaml" device;
    ".gitignore" = pkgs.emptyFile;
  };
}
