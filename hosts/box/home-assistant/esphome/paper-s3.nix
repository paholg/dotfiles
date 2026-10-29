# Builds an M5Stack PaperS3 button panel: a grid of buttons above full-width
# brightness sliders, drawn portrait.
#
# Taps fire `esphome.<name>` events with `control` (and `value` for sliders);
# the generated Home Assistant automations turn those into actions.
#
# Adding a device to Home Assistant isn't declarative: add it in the ESPHome
# integration with its API key, then enable "Allow the device to perform Home
# Assistant actions" in its options, or taps are dropped.
{ lib, pkgs }:
let
  inherit (import ../helpers.nix { inherit lib; }) slugify act;
  npins = import ../../../../npins;
  yaml = pkgs.formats.yaml { tags = true; };

  rotation = 90;
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
  fill = b: color: "id(epd).fill_rect(${i b.x}, ${i b.y}, ${i b.w}, ${i b.h}, ${color});";
  outline =
    b:
    lib.concatMapStringsSep "\n" (edge: fill edge "ink") [
      (b // { h = border; })
      (b // { y = b.y + b.h - border; h = border; })
      (b // { w = border; })
      (b // { x = b.x + b.w - border; w = border; })
    ];
  secret = var: { "!env_var" = var; };
  redraw = {
    "script.execute" = "redraw";
  };
  # Redraws everything with the greyscale waveform, clearing ghosting left by
  # fast refreshes.
  fullRefresh = {
    "script.execute" = "full_refresh";
  };

  # Only the dirty rectangle, in screen coordinates, is drawn and sent to the
  # panel.
  markDirty = b: ''
    id(dirty_x0) = std::min(id(dirty_x0), ${i b.x});
    id(dirty_y0) = std::min(id(dirty_y0), ${i b.y});
    id(dirty_x1) = std::max(id(dirty_x1), ${i (b.x + b.w)});
    id(dirty_y1) = std::max(id(dirty_y1), ${i (b.y + b.h)});
  '';
  markAll = markDirty {
    x = 0;
    y = 0;
    w = width;
    h = height;
  };
  clearDirty = ''
    id(dirty_x0) = ${i width};
    id(dirty_y0) = ${i height};
    id(dirty_x1) = 0;
    id(dirty_y1) = 0;
  '';
  refresh = ''
    {
      const int x0 = id(dirty_x0), y0 = id(dirty_y0), x1 = id(dirty_x1), y1 = id(dirty_y1);
      if (x0 < x1 && y0 < y1) {
        id(epd).refresh(x0, y0, x1 - x0, y1 - y0);
      }
      ${clearDirty}
    }
  '';
  overlapsDirty = b: "${i b.x} < dx1 && ${i (b.x + b.w)} > dx0 && ${i b.y} < dy1 && ${i (b.y + b.h)} > dy0";
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
    let
      box = {
        x = margin + lib.mod n columns * (buttonWidth + gap);
        y = margin + n / columns * (buttonHeight + gap);
        w = buttonWidth;
        h = buttonHeight;
      };
    in
    b // { inherit box; }
  ) buttons;

  placedSliders = lib.imap0 (
    n: s:
    let
      y = margin + buttonsHeight + gap + n * (sliderHeight + gap);
      bar = {
        x = margin;
        y = y + sliderHeight - barHeight;
        w = width - 2 * margin;
        h = barHeight;
      };
    in
    s
    // {
      box = {
        x = margin;
        inherit y;
        w = width - 2 * margin;
        h = sliderHeight;
      };
      inherit bar;
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
      if (${overlapsDirty box}) {
        const bool on = ${on};
        const auto fg = on ? paper : ink;
        if (on) {
          id(epd).fill_rect(${i box.x}, ${i box.y}, ${i box.w}, ${i box.h}, ink);
        } else {
          ${outline box}
        }
        it.image(${cx}, ${i (box.y + box.h / 2 - 30)}, id(${b.id}_icon), ImageAlign::CENTER, fg);
        it.print(${cx}, ${i (box.y + box.h - 40)}, id(label_font), fg, TextAlign::BOTTOM_CENTER, ${str b.label});
        if (id(${b.id}_pending)) {
          it.filled_circle(${i (box.x + box.w - 32)}, ${i (box.y + 32)}, 12, fg);
        }
      }
    '';

  drawSlider =
    s:
    let
      inherit (s) box bar;
    in
    ''
      if (${overlapsDirty box}) {
        const float v = id(${s.id}_value);
        it.print(${i box.x}, ${i box.y}, id(label_font), ink, TextAlign::TOP_LEFT, ${str s.label});
        it.printf(${i (box.x + box.w)}, ${i box.y}, id(label_font), ink, TextAlign::TOP_RIGHT, "%.0f%%", v);
        ${outline bar}
        id(epd).fill_rect(${i bar.x}, ${i bar.y}, (int) (${i bar.w} * v / 100), ${i bar.h}, ink);
        // The tapped level, as a stripe contrasting with the fill under it.
        const float target = id(${s.id}_target);
        if (target >= 0) {
          const int tx = std::clamp(${i bar.x} + (int) (${i bar.w} * target / 100) - 4, ${i bar.x}, ${i (bar.x + bar.w - 8)});
          id(epd).fill_rect(tx, ${i bar.y}, 8, ${i bar.h}, target <= v ? paper : ink);
        }
      }
    '';

  touchButton = b: ''
    if (${inside b.box}) {
      id(fire).execute(${str b.id}, 0.0f);
      id(${b.id}_pending) = true;
      ${markDirty b.box}
      id(redraw).execute();
      id(expire_pending).execute();
      return;
    }
  '';

  touchSlider = s: ''
    if (${inside s.box}) {
      const float v = roundf(std::clamp((x - ${i s.bar.x}) * 100.0f / ${i s.bar.w}, 0.0f, 100.0f));
      id(fire).execute(${str s.id}, v);
      id(${s.id}_target) = v;
      ${markDirty s.box}
      id(redraw).execute();
      id(expire_pending).execute();
      return;
    }
  '';

  device = {
    esphome = {
      inherit name;
      friendly_name = friendlyName;
      libraries = [ "EPDIY=file://${npins.epdiy}" ];
      on_boot = {
        priority = -100;
        "then" = [
          { lambda = markAll; }
          redraw
        ];
      };
    };

    external_components = [
      {
        source = {
          type = "local";
          path = "${./components}";
        };
        components = [ "paper_s3" ];
      }
    ];

    esp32 = {
      board = "esp32-s3-devkitc-1";
      flash_size = "16MB";
      framework = {
        type = "esp-idf";
        sdkconfig_options.CONFIG_EPD_OUTPUT_LUT_ASSEMBLY = "y";
      };
    };

    psram = {
      mode = "octal";
      speed = "80MHz";
    };

    logger = {
      level = "INFO";
      # Every e-paper refresh blocks for longer than ESPHome's 50ms budget.
      logs.component = "ERROR";
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
      power_save_mode = "none";
      use_address = address;
    };

    time = [
      {
        platform = "homeassistant";
        on_time = [
          {
            hours = 4;
            minutes = 0;
            seconds = 0;
            "then" = [ fullRefresh ];
          }
        ];
      }
    ];

    button = [
      {
        platform = "template";
        name = "Full refresh";
        entity_category = "diagnostic";
        on_press = [ fullRefresh ];
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
              ${lib.concatMapStrings touchButton placedButtons}
              ${lib.concatMapStrings touchSlider placedSliders}
            '';
          }
        ];
      }
    ];

    display = [
      {
        platform = "paper_s3";
        id = "epd";
        inherit rotation;
        update_interval = "never";
        auto_clear_enabled = false;
        # The driver maps color brightness straight to grey level.
        lambda = ''
          const auto ink = Color::BLACK;
          const auto paper = Color::WHITE;
          const int dx0 = id(dirty_x0), dy0 = id(dirty_y0), dx1 = id(dirty_x1), dy1 = id(dirty_y1);
          id(epd).fill_rect(dx0, dy0, dx1 - dx0, dy1 - dy0, paper);
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

    globals =
      # Taps awaiting a state update from Home Assistant.
      map (b: {
        id = "${b.id}_pending";
        type = "bool";
        initial_value = "false";
      }) buttons
      ++ map (s: {
        id = "${s.id}_target";
        type = "float";
        initial_value = "-1";
      }) sliders
      ++ lib.mapAttrsToList
      (id: initial_value: {
        inherit id initial_value;
        type = "int";
      })
      {
        dirty_x0 = i width;
        dirty_y0 = i height;
        dirty_x1 = "0";
        dirty_y1 = "0";
      }
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
          on_state = [
            {
              lambda = ''
                id(${b.id}_pending) = false;
                ${markDirty b.box}
              '';
            }
            redraw
          ];
        }
      ) (lib.filter (b: b ? state) placedButtons)
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
                id(${s.id}_target) = -1;
                ${markDirty s.box}
              '';
            }
            redraw
          ];
        }
      ) placedSliders;

    sensor = map (
      s:
      homeassistant {
        id = "${s.id}_brightness";
        entity_id = s.light.entity_id;
        attribute = "brightness";
        on_value = [
          {
            lambda = ''
              if (id(${s.id}_on).state) id(${s.id}_value) = x * 100 / 255;
              id(${s.id}_target) = -1;
              ${markDirty s.box}
            '';
          }
          redraw
        ];
      }
    ) placedSliders;

    script = [
      {
        id = "full_refresh";
        "then" = [
          {
            lambda = ''
              ${markAll}
              id(epd).update();
              ${clearDirty}
            '';
          }
        ];
      }
      # Coalesces bursts of state updates into one refresh, and lets the main
      # loop send a tap's event before the refresh blocks it.
      {
        id = "redraw";
        mode = "restart";
        "then" = [
          { delay = "20ms"; }
          { lambda = refresh; }
        ];
      }
      # Clears marks for taps that never got a state update, such as stateless
      # buttons or a slider tapped at its current level.
      {
        id = "expire_pending";
        mode = "restart";
        "then" = [
          { delay = "3s"; }
          {
            lambda = ''
              ${lib.concatMapStrings (b: ''
                if (id(${b.id}_pending)) {
                  id(${b.id}_pending) = false;
                  ${markDirty b.box}
                }
              '') placedButtons}
              ${lib.concatMapStrings (s: ''
                if (id(${s.id}_target) >= 0) {
                  id(${s.id}_target) = -1;
                  ${markDirty s.box}
                }
              '') placedSliders}
              ${refresh}
            '';
          }
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
              data = {
                control."!lambda" = "return control;";
                value."!lambda" = "return str_sprintf(\"%.0f\", value);";
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
