# A Seeed reTerminal E1003 (10.3" e-paper, 1872x1404) wall dashboard: clock,
# date, current weather, sunrise/sunset, air quality, UV index, and a chart of
# temperature and chance of rain over the next 12 hours. Meant to stay on USB
# power, as it redraws every minute.
#
# The layout follows https://github.com/lmarzen/esp32-weather-epd.
#
# Home Assistant's forecast has no chance of rain, air quality or UV index, so
# everything but the condition icon comes from Open-Meteo, for the home zone;
# temperatures from two forecasts would disagree.
{ lib, pkgs, ... }:
let
  inherit (import ../helpers.nix { inherit lib; }) slugify;
  inherit (import ../entities.nix { inherit lib; }) weather;
  yaml = pkgs.formats.yaml { tags = true; };

  name = "dashboard-reterminal";
  friendlyName = "Dashboard reTerminal";
  address = "10.0.0.16";

  width = 1872;
  height = 1404;
  margin = 80;
  # The chart's plot area, between its axis labels.
  plot = {
    x = 190;
    y = 800;
    w = 1470;
    h = 470;
  };
  chartHours = 12;

  i = toString;
  str = builtins.toJSON;
  secret = var: { "!env_var" = var; };
  redraw = {
    "script.execute" = "redraw";
  };

  # Icons for Home Assistant's weather conditions.
  conditions = {
    clear-night = "mdi:weather-night";
    cloudy = "mdi:weather-cloudy";
    exceptional = "mdi:alert-circle-outline";
    fog = "mdi:weather-fog";
    hail = "mdi:weather-hail";
    lightning = "mdi:weather-lightning";
    lightning-rainy = "mdi:weather-lightning-rainy";
    partlycloudy = "mdi:weather-partly-cloudy";
    pouring = "mdi:weather-pouring";
    rainy = "mdi:weather-rainy";
    snowy = "mdi:weather-snowy";
    snowy-rainy = "mdi:weather-snowy-rainy";
    sunny = "mdi:weather-sunny";
    windy = "mdi:weather-windy";
    windy-variant = "mdi:weather-windy-variant";
  };
  conditionId = condition: "condition_${slugify condition}";

  # Home Assistant sensors made for this device, by ESPHome id.
  sensors = lib.mapAttrs (_: suffix: "${friendlyName} ${suffix}") {
    sunrise = "Sunrise";
    sunset = "Sunset";
    # Hourly values from the current hour, comma-separated.
    temperatures = "Temperatures";
    rain_chances = "Rain Chances";
    # The hour of day of the first of those.
    first_hour = "First Hour";
    uv = "UV Index";
    temperature = "Temperature";
    today_high = "High";
    today_low = "Low";
    aqi = "AQI";
  };
  sensorEntity = id: "sensor.${slugify sensors.${id}}";
  haSensor =
    id: attrs:
    {
      name = sensors.${id};
      unique_id = slugify sensors.${id};
    }
    // attrs;
  restSensor = id: value_template: haSensor id { inherit value_template; };
  # Once the sun is up, this is tomorrow's sunrise, which is within a couple of
  # minutes of today's.
  sunTime =
    attribute: "{{ as_timestamp(state_attr('sun.sun', '${attribute}')) | timestamp_custom('%H:%M') }}";

  # Keeps the coordinates out of this repo.
  home = "latitude={{ state_attr('zone.home', 'latitude') }}&longitude={{ state_attr('zone.home', 'longitude') }}&timezone=auto";
  homeassistant =
    attrs:
    {
      platform = "homeassistant";
      internal = true;
      on_value = [ redraw ];
    }
    // attrs;
  fromSensor =
    id:
    homeassistant {
      inherit id;
      entity_id = sensorEntity id;
    };

  icon = id: file: size: {
    platform = "file";
    inherit id file;
    resize = "${i size}x${i size}";
    type = "binary";
    transparency = "chroma_key";
  };
  font =
    id: size: glyphs:
    {
      file = {
        type = "gfonts";
        family = "Roboto";
        weight = 500;
      };
      inherit id size;
    }
    // lib.optionalAttrs (glyphs != null) { inherit glyphs; };

  # The row of air quality, UV index, and today's high and low.
  infoY = 660;

  device = {
    esphome = {
      inherit name;
      friendly_name = friendlyName;
      # Shows the waiting message until Home Assistant connects. A greyscale
      # refresh, as a fast one leaves whatever the panel showed before.
      on_boot = {
        priority = -100;
        "then" = [ { lambda = "id(epd).update_mode(it8951::UPDATE_MODE_GC16);"; } ];
      };
    };

    esp32 = {
      board = "esp32-s3-devkitc-1";
      # The flash is 32MB, but OTA beyond 16MB needs experimental ESP-IDF features.
      flash_size = "16MB";
      framework.type = "esp-idf";
    };

    # The framebuffer lives in PSRAM.
    psram = {
      mode = "octal";
      speed = "80MHz";
    };

    logger = {
      level = "INFO";
      # The USB port is a CH340 serial bridge on UART0, not the ESP32's own USB.
      hardware_uart = "UART0";
    };

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
      use_address = address;
    };

    time = [
      {
        platform = "homeassistant";
        id = "ha_time";
        on_time_sync = [ redraw ];
        on_time = [
          {
            seconds = 0;
            "then" = [ redraw ];
          }
        ];
      }
    ];

    spi = {
      clk_pin = "GPIO7";
      mosi_pin = "GPIO9";
      miso_pin = "GPIO8";
    };

    display = [
      {
        platform = "it8951";
        id = "epd";
        # Supplies the panel's other pins, power rails and VCOM.
        model = "seeed-reterminal-e1003";
        # Monochrome with the fast waveform redraws without the flashing of a
        # greyscale refresh; the periodic full refresh clears the ghosting that
        # builds up.
        grayscale = false;
        dithering = false;
        update_mode = "DU";
        full_update_every = 30;
        update_interval = "never";
        lambda = ''
          const auto ink = Color::BLACK;
          auto now = id(ha_time).now();
          if (!now.is_valid()) {
            it.print(${i (width / 2)}, ${i (height / 2)}, id(date_font), ink, TextAlign::CENTER, "Waiting for Home Assistant");
            return;
          }

          it.printf(${i margin}, 370, id(clock_font), ink, TextAlign::CENTER_LEFT, "%02d:%02d", now.hour, now.minute);
          const std::string date = now.strftime("%A, %B ") + to_string(now.day_of_month);
          it.print(${i margin}, 90, id(date_font), ink, TextAlign::CENTER_LEFT, date.c_str());

          // The right column, from the top: sunrise, sunset, the current
          // temperature beside its icon, then today's high and low.
          {
            const int right = ${i (width - margin)};
            const char *rise = id(sunrise).state.c_str(), *set = id(sunset).state.c_str();
            int x0, x1, y, w, h;
            it.get_text_bounds(right, 70, rise, id(info_font), TextAlign::CENTER_RIGHT, &x0, &y, &w, &h);
            it.get_text_bounds(right, 180, set, id(info_font), TextAlign::CENTER_RIGHT, &x1, &y, &w, &h);
            // The icons share a column, against the wider of the two times.
            const int icons = std::min(x0, x1) - 75;
            it.print(right, 70, id(info_font), ink, TextAlign::CENTER_RIGHT, rise);
            it.image(icons, 70, id(sunrise_icon), ImageAlign::CENTER, ink);
            it.print(right, 180, id(info_font), ink, TextAlign::CENTER_RIGHT, set);
            it.image(icons, 180, id(sunset_icon), ImageAlign::CENTER, ink);

            // The icon sits against the temperature, whatever its width.
            int icon = right - 420;
            const float current = id(temperature).state;
            if (!isnan(current)) {
              char text[8];
              snprintf(text, sizeof(text), "%.0f°", current);
              it.get_text_bounds(right, 490, text, id(temperature_font), TextAlign::CENTER_RIGHT, &icon, &y, &w, &h);
              it.print(right, 490, id(temperature_font), ink, TextAlign::CENTER_RIGHT, text);
            }
            const std::string condition = id(current_condition).state;
            ${lib.concatStrings (
              lib.mapAttrsToList (condition: _: ''
                if (condition == ${str condition}) it.image(icon - 150, 490, id(${conditionId condition}), ImageAlign::CENTER, ink);
              '') conditions
            )}
            if (!isnan(id(today_high).state) && !isnan(id(today_low).state)) {
              it.printf(right, ${i infoY}, id(info_font), ink, TextAlign::CENTER_RIGHT, "%.0f° / %.0f°", id(today_high).state, id(today_low).state);
            }
          }

          if (!isnan(id(aqi).state)) {
            it.printf(${i margin}, ${i infoY}, id(info_font), ink, TextAlign::CENTER_LEFT, "AQI %.0f", id(aqi).state);
          }
          if (!isnan(id(uv).state)) {
            it.printf(${i (margin + 420)}, ${i infoY}, id(info_font), ink, TextAlign::CENTER_LEFT, "UV %.0f", id(uv).state);
          }

          // Temperature as a line over shaded bars for the chance of rain.
          const auto numbers = [](const std::string &s) {
            std::vector<float> values;
            const char *p = s.c_str();
            while (*p != 0) {
              char *end;
              const float value = strtof(p, &end);
              if (end == p) break;
              values.push_back(value);
              p = *end == ',' ? end + 1 : end;
            }
            return values;
          };
          const auto temps = numbers(id(temperatures).state);
          const auto chances = numbers(id(rain_chances).state);
          const int points = ${i (chartHours + 1)};
          if ((int) temps.size() < points || (int) chances.size() < points || isnan(id(first_hour).state)) return;

          const int px = ${i plot.x}, py = ${i plot.y}, pw = ${i plot.w}, ph = ${i plot.h};
          const float step = pw / (float) (points - 1);
          const auto column = [&](int n) { return px + (int) roundf(n * step); };

          float low = temps[0], high = temps[0];
          for (int n = 1; n < points; n++) {
            low = std::min(low, temps[n]);
            high = std::max(high, temps[n]);
          }
          // Four intervals of a round size, so that the gridlines also suit
          // the chance of rain, with room above and below the line.
          const int ticks = 4;
          int tick = 0, bottom = 0;
          do {
            tick += 5;
            bottom = (int) floorf((low - 1) / tick) * tick;
          } while (bottom + ticks * tick < high + 1);
          const int top = bottom + ticks * tick;
          const auto row = [&](float t) { return py + ph - (int) roundf((t - bottom) * ph / (top - bottom)); };

          // Each bar spans the hour around its point, as a 25% grey stipple.
          for (int n = 0; n < points; n++) {
            const int x0 = std::max(px, column(n) - (int) (step / 2));
            const int x1 = std::min(px + pw, column(n) + (int) (step / 2));
            const int y0 = py + ph - (int) roundf(std::clamp(chances[n], 0.0f, 100.0f) * ph / 100);
            for (int y = y0; y < py + ph; y++) {
              if ((y & 3) >= 2) continue;
              for (int x = x0; x < x1; x++) {
                if ((x & 3) < 2) it.draw_pixel_at(x, y, ink);
              }
            }
          }

          for (int n = 0; n <= ticks; n++) {
            const int y = row(bottom + n * tick);
            if (n > 0) {
              for (int x = px; x < px + pw; x += 16) it.filled_rectangle(x, y, 6, 2, ink);
            }
            it.printf(px - 24, y, id(axis_font), ink, TextAlign::CENTER_RIGHT, "%d°", bottom + n * tick);
            it.printf(px + pw + 24, y, id(axis_font), ink, TextAlign::CENTER_LEFT, "%d%%", n * 100 / ticks);
          }
          it.filled_rectangle(px, py + ph, pw, 4, ink);

          for (int n = 0; n < points; n += 3) {
            const int hour = ((int) id(first_hour).state + n) % 24;
            it.filled_rectangle(column(n) - 2, py + ph, 4, 16, ink);
            it.printf(column(n), py + ph + 60, id(axis_font), ink, TextAlign::CENTER, "%02d:00", hour);
          }

          for (int n = 0; n + 1 < points; n++) {
            const int x0 = column(n), x1 = column(n + 1);
            const int y0 = row(temps[n]), y1 = row(temps[n + 1]);
            for (int x = x0; x <= x1; x++) {
              it.filled_circle(x, y0 + (y1 - y0) * (x - x0) / (x1 - x0), 4, ink);
            }
          }
        '';
      }
    ];

    font = [
      (font "clock_font" 340 "0123456789:")
      (font "temperature_font" 220 "0123456789-°")
      (font "date_font" 100 null)
      (font "info_font" 76 null)
      (font "axis_font" 56 null)
    ];

    image = [
      (icon "sunrise_icon" "mdi:weather-sunset-up" 110)
      (icon "sunset_icon" "mdi:weather-sunset-down" 110)
    ]
    ++ lib.mapAttrsToList (condition: file: icon (conditionId condition) file 240) conditions;

    sensor = [
      (fromSensor "temperature")
      (fromSensor "first_hour")
      (fromSensor "uv")
      (fromSensor "today_high")
      (fromSensor "today_low")
      (fromSensor "aqi")
    ];

    text_sensor = [
      (homeassistant {
        id = "current_condition";
        inherit (weather.home) entity_id;
      })
      (fromSensor "sunrise")
      (fromSensor "sunset")
      (fromSensor "temperatures")
      (fromSensor "rain_chances")
    ];

    script = [
      # Coalesces the burst of state updates on connecting into one refresh.
      {
        id = "redraw";
        mode = "restart";
        "then" = [
          { delay = "500ms"; }
          { "component.update" = "epd"; }
        ];
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

  services.home-assistant.config = {
    template = [
      {
        sensor = [
          (haSensor "sunrise" { state = sunTime "next_rising"; })
          (haSensor "sunset" { state = sunTime "next_setting"; })
        ];
      }
    ];

    # https://open-meteo.com/en/docs and https://open-meteo.com/en/docs/air-quality-api
    rest = [
      {
        resource_template = "https://api.open-meteo.com/v1/forecast?${home}&current=temperature_2m&hourly=temperature_2m,precipitation_probability&daily=uv_index_max,temperature_2m_max,temperature_2m_min&forecast_hours=${i (chartHours + 1)}&forecast_days=1&temperature_unit=fahrenheit";
        scan_interval = 900;
        sensor = [
          (restSensor "temperatures" "{{ value_json.hourly.temperature_2m | map('round') | map('int') | join(',') }}")
          (restSensor "rain_chances" "{{ value_json.hourly.precipitation_probability | join(',') }}")
          (restSensor "first_hour" "{{ as_datetime(value_json.hourly.time[0]).hour }}")
          (restSensor "uv" "{{ value_json.daily.uv_index_max[0] | round }}")
          (restSensor "temperature" "{{ value_json.current.temperature_2m | round }}")
          (restSensor "today_high" "{{ value_json.daily.temperature_2m_max[0] | round }}")
          (restSensor "today_low" "{{ value_json.daily.temperature_2m_min[0] | round }}")
        ];
      }
      {
        resource_template = "https://air-quality-api.open-meteo.com/v1/air-quality?${home}&current=us_aqi";
        scan_interval = 900;
        sensor = [
          (restSensor "aqi" "{{ value_json.current.us_aqi }}")
        ];
      }
    ];
  };
}
