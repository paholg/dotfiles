# Builds an M5Stack PaperS3 button panel: a grid of buttons above full-width
# brightness sliders, drawn portrait.
#
# Taps fire `esphome.<name>` events with `control` (and `value` for sliders);
# the generated Home Assistant automations turn those into actions.
{ lib, pkgs }:
let
  inherit (import ../helpers.nix { inherit lib; }) slugify act;
  npins = import ../../../../npins;
  yaml = pkgs.formats.yaml { tags = true; };

  # Panel pixels, before rotation.
  panelWidth = 960;
  panelHeight = 540;

  width = 540;
  height = 960;
  margin = 24;
  gap = 24;
  sliderHeight = 140;
  barHeight = 88;
  border = 4;

  i = toString;
  # C++ string literal.
  str = builtins.toJSON;
  inside = b: "x >= ${i b.x} && x < ${i (b.x + b.w)} && y >= ${i b.y} && y < ${i (b.y + b.h)}";
  outline =
    b:
    "for (int t = 0; t < ${i border}; t++) it.rectangle(${i b.x} + t, ${i b.y} + t, ${i b.w} - 2 * t, ${i b.h} - 2 * t, ink);";
  secret = var: { "!env_var" = var; };
  redraw = {
    "script.execute" = "redraw";
  };
  homeassistant =
    attrs:
    {
      platform = "homeassistant";
      internal = true;
    }
    // attrs;
in
{
  name,
  friendlyName,
  # Reserved in DHCP; used for OTA updates and logs.
  address,
  buttons ? [ ],
  sliders ? [ ],
  columns ? 1,
}:
let
  event = "esphome.${slugify name}";

  buttonsHeight = height - 2 * margin - lib.length sliders * (sliderHeight + gap);
  rows = (lib.length buttons + columns - 1) / columns;
  buttonWidth = (width - 2 * margin - (columns - 1) * gap) / columns;
  buttonHeight = (buttonsHeight - (rows - 1) * gap) / rows;

  placedButtons = lib.imap0 (
    n: b:
    b
    // {
      index = n;
      box = {
        x = margin + lib.mod n columns * (buttonWidth + gap);
        y = margin + n / columns * (buttonHeight + gap);
        w = buttonWidth;
        h = buttonHeight;
      };
    }
  ) buttons;

  placedSliders = lib.imap0 (
    n: s:
    let
      y = margin + buttonsHeight + gap + n * (sliderHeight + gap);
    in
    s
    // {
      box = {
        x = margin;
        inherit y;
        w = width - 2 * margin;
        h = sliderHeight;
      };
      bar = {
        x = margin;
        y = y + sliderHeight - barHeight;
        w = width - 2 * margin;
        h = barHeight;
      };
    }
  ) sliders;

  drawButton =
    b:
    let
      inherit (b) box;
      cx = i (box.x + box.w / 2);
      on = if b ? state then "id(${b.id}_on).state" else "false";
    in
    ''
      {
        // A pressed button is drawn inverted.
        const bool on = ${on} != (id(pressed_button) == ${i b.index});
        const auto fg = on ? paper : ink;
        if (on) {
          it.filled_rectangle(${i box.x}, ${i box.y}, ${i box.w}, ${i box.h}, ink);
        } else {
          ${outline box}
        }
        it.image(${cx}, ${i (box.y + box.h / 2 - 30)}, id(${b.id}_icon), ImageAlign::CENTER, fg);
        it.print(${cx}, ${i (box.y + box.h - 40)}, id(label_font), fg, TextAlign::BOTTOM_CENTER, ${str b.label});
      }
    '';

  drawSlider =
    s:
    let
      inherit (s) box bar;
    in
    ''
      {
        const float v = id(${s.id}_value);
        it.print(${i box.x}, ${i box.y}, id(label_font), ink, TextAlign::TOP_LEFT, ${str s.label});
        it.printf(${i (box.x + box.w)}, ${i box.y}, id(label_font), ink, TextAlign::TOP_RIGHT, "%.0f%%", v);
        ${outline bar}
        it.filled_rectangle(${i bar.x}, ${i bar.y}, (int) (${i bar.w} * v / 100), ${i bar.h}, ink);
      }
    '';

  touchButton = b: ''
    if (${inside b.box}) {
      id(pressed_button) = ${i b.index};
      id(flash).execute();
      id(fire).execute(${str b.id}, 0.0f);
      return;
    }
  '';

  # Shows the tapped value immediately rather than waiting on Home Assistant.
  touchSlider = s: ''
    if (${inside s.box}) {
      const float v = roundf(std::clamp((x - ${i s.bar.x}) * 100.0f / ${i s.bar.w}, 0.0f, 100.0f));
      id(${s.id}_value) = v;
      id(redraw).execute();
      id(fire).execute(${str s.id}, v);
      return;
    }
  '';

  device = {
    esphome = {
      inherit name;
      friendly_name = friendlyName;
      build_flags = [ "-DBOARD_HAS_PSRAM" ];
      libraries = [ "EPDIY=file://${npins.epdiy}" ];
      on_boot = {
        priority = -100;
        "then" = [ redraw ];
      };
    };

    external_components = [
      {
        source = {
          type = "local";
          path = "${npins.esphome_components}/components";
        };
        components = [ "ed047tc1" ];
      }
    ];

    esp32 = {
      board = "esp32-s3-devkitc-1";
      flash_size = "16MB";
      framework = {
        type = "esp-idf";
        sdkconfig_options = {
          CONFIG_EPD_OUTPUT_LUT_ASSEMBLY = "y";
          CONFIG_SPIRAM_USE_MALLOC = "y";
          CONFIG_SPIRAM_MALLOC_ALWAYSINTERNAL = "16384";
        };
      };
    };

    psram = {
      mode = "octal";
      speed = "80MHz";
    };

    logger = { };

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

    # A full refresh clears ghosting left by partial updates.
    time = [
      {
        platform = "homeassistant";
        on_time = [
          {
            hours = 4;
            minutes = 0;
            seconds = 0;
            "then" = [ { lambda = "id(epd).update();"; } ];
          }
        ];
      }
    ];

    i2c = {
      sda = "GPIO41";
      scl = "GPIO42";
      frequency = "200kHz";
    };

    touchscreen = [
      {
        platform = "gt911";
        id = "touch";
        display = "epd";
        interrupt_pin = "GPIO48";
        transform = {
          mirror_x = true;
          mirror_y = true;
        };
        on_touch = [
          {
            lambda = ''
              const int x = touch.x, y = touch.y;
              ESP_LOGD("touch", "x=%d y=%d", x, y);
              ${lib.concatMapStrings touchButton placedButtons}
              ${lib.concatMapStrings touchSlider placedSliders}
            '';
          }
        ];
      }
    ];

    display = [
      {
        platform = "ed047tc1";
        id = "epd";
        pwr_pin = "GPIO45";
        bst_en_pin = "GPIO46";
        xstl_pin = "GPIO13";
        xle_pin = "GPIO15";
        spv_pin = "GPIO17";
        ckv_pin = "GPIO18";
        pclk_pin = "GPIO16";
        d0_pin = "GPIO6";
        d1_pin = "GPIO14";
        d2_pin = "GPIO7";
        d3_pin = "GPIO12";
        d4_pin = "GPIO9";
        d5_pin = "GPIO11";
        d6_pin = "GPIO8";
        d7_pin = "GPIO10";
        rotation = 90;
        update_interval = "never";
        # The driver maps color brightness straight to grey level.
        lambda = ''
          const auto ink = Color::BLACK;
          const auto paper = Color::WHITE;
          it.fill(paper);
          ${lib.concatMapStrings drawButton placedButtons}
          ${lib.concatMapStrings drawSlider placedSliders}
        '';
      }
    ];

    font = [
      {
        file = {
          type = "gfonts";
          family = "Roboto";
          weight = 500;
        };
        id = "label_font";
        size = 40;
      }
    ];

    image = map (b: {
      platform = "file";
      file = b.icon;
      id = "${b.id}_icon";
      resize = "144x144";
      type = "binary";
      transparency = "chroma_key";
    }) buttons;

    globals = [
      {
        id = "pressed_button";
        type = "int";
        initial_value = "-1";
      }
    ]
    ++ map (s: {
      id = "${s.id}_value";
      type = "float";
      initial_value = "0";
    }) sliders;

    binary_sensor =
      map (
        b:
        homeassistant {
          id = "${b.id}_on";
          entity_id = b.state.entity_id;
          on_state = [ redraw ];
        }
      ) (lib.filter (b: b ? state) buttons)
      # Brightness is unset while a light is off, so on/off is tracked separately.
      ++ map (
        s:
        homeassistant {
          id = "${s.id}_on";
          entity_id = s.light.entity_id;
          on_state = [
            {
              lambda = ''
                const float b = id(${s.id}_brightness).state;
                id(${s.id}_value) = x && !isnan(b) ? b * 100 / 255 : 0;
              '';
            }
            redraw
          ];
        }
      ) sliders;

    sensor = map (
      s:
      homeassistant {
        id = "${s.id}_brightness";
        entity_id = s.light.entity_id;
        attribute = "brightness";
        on_value = [
          { lambda = "if (id(${s.id}_on).state) id(${s.id}_value) = x * 100 / 255;"; }
          redraw
        ];
      }
    ) sliders;

    script = [
      # Coalesces bursts of state updates into one refresh.
      {
        id = "redraw";
        mode = "restart";
        "then" = [
          { delay = "100ms"; }
          { lambda = "id(epd).partial_update(0, 0, ${i panelWidth}, ${i panelHeight});"; }
        ];
      }
      {
        id = "flash";
        mode = "restart";
        "then" = [
          { lambda = "id(epd).partial_update(0, 0, ${i panelWidth}, ${i panelHeight});"; }
          { delay = "300ms"; }
          { lambda = "id(pressed_button) = -1;"; }
          redraw
        ];
      }
      {
        id = "fire";
        parameters = {
          control = "string";
          value = "float";
        };
        "then" = [
          {
            "homeassistant.event" = {
              inherit event;
              data_template = {
                control = "{{ control }}";
                value = "{{ value }}";
              };
              variables = {
                control = "return control;";
                value = "return value;";
              };
            };
          }
        ];
      }
    ];
  };

  automation =
    control: label: action:
    let
      alias = "${friendlyName} ${label}";
    in
    {
      inherit alias;
      id = slugify alias;
      triggers = [
        {
          trigger = "event";
          event_type = event;
          event_data = { inherit control; };
        }
      ];
      actions = [ action ];
    };
in
{
  # ESPHome writes a .gitignore beside the config unless one exists.
  custom.esphome.${name} = pkgs.linkFarm "esphome-${name}" {
    "${name}.yaml" = yaml.generate "${name}.yaml" device;
    ".gitignore" = pkgs.emptyFile;
  };

  services.home-assistant.config.automation =
    map (b: automation b.id b.label b.action) buttons
    ++ map (
      s:
      automation s.id s.label (
        act "turn_on" s.light { brightness_pct = "{{ trigger.event.data.value | int }}"; }
      )
    ) sliders;
}
