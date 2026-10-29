{ lib }:
let
  inherit (import ./helpers.nix { inherit lib; })
    slugify
    entity
    act
    lowerTo
    on
    states
    ;

  mk = domain: lib.mapAttrs (_: entity domain);

  # Entities defined here carry their Home Assistant config in `config`.
  # Scenes and groups get entity ids from their names; scripts and timers from their keys.
  mkScene =
    name: icon: entities:
    entity "scene" (slugify name)
    // {
      config = {
        id = slugify name;
        inherit name icon entities;
      };
    };
  mkLightGroup =
    name: members:
    entity "light" (slugify name)
    // {
      config = {
        platform = "group";
        unique_id = slugify name;
        inherit name;
        entities = map (e: e.entity_id) members;
      };
    };
  mkKeyed =
    domain: id: config:
    entity domain id // { inherit config; };

  # Zigbee2MQTT publishes button presses to `zigbee2mqtt/<friendly name>/action`
  # with the action as payload.
  mkRemote =
    name: actions:
    lib.mapAttrs (_: payload: {
      trigger = "mqtt";
      topic = "zigbee2mqtt/${name}/action";
      inherit payload;
    }) actions;

  bulbs = mk "light" {
    bed = "0xa4c138216ba5b396";
    canNorth = "0x0022a3000035fdad";
    canNorthEast = "0x0022a30000343bb0";
    canSouth = "living_room_can_light_south";
    dining = "0x8c8b48fffe4b29bc";
    kitchen = "0x8c8b48fffe4d17ad";
    mudRoom = "0x04e3e5fffed149b0";
    porch = "0x8c8b48fffe4b3ddf";
    stairs = "0x8c8b48fffe4b897a";
    tv = "liv_tv_backlight";
    window = "0x0022a3000035fbf4";
  };

  light = bulbs // {
    cans = mkLightGroup "Can Lights" (
      with bulbs;
      [
        canNorth
        canNorthEast
        canSouth
      ]
    );
  };

  switch = mk "switch" {
    espresso = "0x00124b002f8d8879";
    minifridge = "0x00124b002f89e467";
  };

  todo = mk "todo" { shopping = "shopping_list"; };

  vacuum = mk "vacuum" { q5 = "q5_max"; };

  weather = mk "weather" { home = "forecast_home"; };

  remote = {
    buttons = mkRemote "Buttons" {
      one = "1_single";
      four = "4_single";
    };
    desk = mkRemote "Desk Buttons" { one = "single_button_1"; };
    dining = mkRemote "Dining Room" { config = "config_single"; };
    knob = mkRemote "Knob" {
      left = "rotate_left";
      right = "rotate_right";
    };
    stairs = mkRemote "Stair Lights" {
      config = "config_single";
      configDouble = "config_double";
    };
  };

  scene = {
    morning = mkScene "Wake Up" "mdi:white-balance-sunny" (
      states (with light; [
        stairs
        dining
        kitchen
        window
        canNorth
        canNorthEast
        canSouth
      ]) (on 255)
      // states [ switch.espresso ] "on"
    );
    sleepy = mkScene "Sleepy Time" "mdi:weather-night" (
      states (with light; [
        stairs
        dining
        mudRoom
        kitchen
        window
        tv
        canNorth
        canNorthEast
        canSouth
        switch.espresso
      ]) "off"
      // states [ light.porch ] (on 51)
    );
    tv = mkScene "TV Time" "mdi:movie-open" (
      with light;
      states [ canNorth canSouth ] (on 153)
      // states [ canNorthEast window ] "off"
      // states [ tv ] (on 26 // { color_temp_kelvin = 2000; })
    );
  };

  timer.minifridge = mkKeyed "timer" "minifridge" {
    name = "Minifridge Timer";
    icon = "mdi:fridge-industrial-outline";
    duration = "1:00:00";
  };

  script.minifridgeSnooze = mkKeyed "script" "minifridge_snooze" {
    alias = "Minifridge Snooze";
    icon = "mdi:alarm-snooze";
    sequence = [
      (act "turn_off" switch.minifridge { })
      (act "start" timer.minifridge { })
    ];
  };

  script.tvTime = mkKeyed "script" "tv_time" {
    alias = "TV Time";
    icon = "mdi:movie-open";
    sequence = [
      (act "turn_on" scene.tv { })
      (lowerTo light.dining 92)
    ];
  };
in
{
  inherit
    light
    switch
    todo
    vacuum
    weather
    remote
    scene
    timer
    script
    ;
}
