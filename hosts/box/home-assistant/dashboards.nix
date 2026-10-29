{ lib, pkgs, ... }:
let
  inherit (import ./entities.nix { inherit lib; })
    light
    switch
    scene
    script
    todo
    vacuum
    weather
    ;

  yaml = pkgs.formats.yaml { };

  speakeasyPath = "dashboard-speakeasy";

  moreInfo.action = "more-info";

  lightCard =
    entity: extra:
    {
      type = "custom:mushroom-light-card";
      entity = entity.entity_id;
      show_brightness_control = true;
      use_light_color = false;
      collapsible_controls = false;
      fill_container = false;
      double_tap_action = moreInfo;
      hold_action = moreInfo;
    }
    // extra;
  tempLightCard =
    entity:
    lightCard entity {
      use_light_color = true;
      show_color_control = false;
      show_color_temp_control = true;
    };

  button =
    entity: extra:
    {
      type = "button";
      entity = entity.entity_id;
      show_name = false;
      show_icon = true;
    }
    // extra;
  sceneButton =
    entity: extra:
    button entity (
      {
        tap_action.action = "toggle";
        show_state = false;
      }
      // extra
    );

  grid = columns: cards: {
    type = "grid";
    square = false;
    inherit columns cards;
  };

  # Each room gets an area card on the overview linking to a subview of its cards.
  rooms = [
    {
      area = "front_of_house";
      title = "Front of House";
      color = "light-blue";
      cards = [
        (lightCard light.porch { })
        (lightCard light.mudRoom { })
      ];
    }
    {
      area = "dining_room";
      title = "Dining Room";
      color = "brown";
      cards = [
        (lightCard light.dining {
          use_light_color = true;
          name = "Overhead";
        })
        (lightCard light.window { name = "Window"; })
        (button switch.espresso { name = "Espresso"; })
        (button vacuum.q5 { name = "Vacuum"; })
        (lightCard light.kitchen { })
      ];
    }
    {
      area = "living_room";
      title = "Living Room";
      color = "green";
      cards = [
        (tempLightCard light.tv)
        (lightCard light.stairs { })
        (lightCard light.cans { name = "Ambient"; })
        (lightCard light.canNorth { })
        (lightCard light.canSouth { })
        (lightCard light.canNorthEast { })
      ];
    }
    {
      area = "upstairs";
      title = "Upstairs";
      color = "purple";
      cards = [
        (lightCard light.stairs { fill_container = true; })
        (tempLightCard light.bed)
        (button switch.minifridge {
          show_name = true;
          name = "Snooze Fridge";
          icon = "mdi:alarm-snooze";
          tap_action = {
            action = "perform-action";
            perform_action = script.minifridgeSnooze.entity_id;
          };
        })
        (button switch.minifridge { show_name = true; })
      ];
    }
  ];

  roomPath = room: builtins.replaceStrings [ "_" ] [ "-" ] room.area;

  areaCard = room: {
    type = "area";
    inherit (room) area color;
    display_type = "icon";
    alert_classes = [
      "motion"
      "moisture"
    ];
    sensor_classes = [
      "temperature"
      "humidity"
    ];
    features_position = "bottom";
    vertical = false;
    tap_action = {
      action = "navigate";
      navigation_path = "/${speakeasyPath}/${roomPath room}";
    };
    card_mod.style = "ha-card ha-tile-container { display: none; }";
  };

  roomView = room: {
    type = "masonry";
    subview = true;
    inherit (room) title;
    path = roomPath room;
    cards = [ (grid 2 room.cards) ];
  };

  overview = {
    type = "masonry";
    path = "";
    title = "Overview";
    background = {
      opacity = 20;
      alignment = "center";
      size = "cover";
      repeat = "repeat";
      attachment = "fixed";
    };
    cards = [
      {
        type = "custom:hourly-weather";
        entity = weather.home.entity_id;
        name = " ";
        num_segments = 11;
        label_spacing = 2;
        show_date = false;
        show_precipitation_amounts = true;
        show_precipitation_probability = true;
        icons = true;
        colors.sunny = "#ffbd31";
      }
      (grid 3 [
        (sceneButton scene.morning { color = "accent"; })
        (sceneButton script.sleepyTime { color = "indigo"; })
        (sceneButton script.tvTime { })
      ])
      ((grid 4 (map areaCard rooms)) // { square = true; })
      {
        type = "todo-list";
        entity = todo.shopping.entity_id;
        title = "Shopping List";
        display_order = "none";
        hide_completed = true;
        hide_section_headers = true;
      }
    ];
  };

  onThings = {
    type = "panel";
    path = "on-things";
    title = "On Things";
    cards = [
      {
        type = "custom:auto-entities";
        filter.include =
          map
            (domain: {
              inherit domain;
              state = "on";
            })
            [
              "light"
              "switch"
            ];
        group_by = "area";
        card.type = "entities";
      }
    ];
  };
in
{
  services.home-assistant.config.lovelace.dashboards = {
    ${speakeasyPath} = {
      mode = "yaml";
      title = "Speakeasy";
      icon = "mdi:home";
      show_in_sidebar = true;
      filename = "${yaml.generate "speakeasy.yaml" {
        views = [ overview ] ++ map roomView rooms ++ [ onThings ];
      }}";
    };
    dashboard-map = {
      mode = "yaml";
      title = "Map";
      icon = "mdi:map";
      show_in_sidebar = true;
      filename = "${yaml.generate "map.yaml" { strategy.type = "map"; }}";
    };
  };
}
