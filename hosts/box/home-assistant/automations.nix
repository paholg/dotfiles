{ lib, ... }:
let
  inherit (import ./helpers.nix { inherit lib; }) slugify act;
  entities = import ./entities.nix { inherit lib; };
  inherit (entities)
    light
    switch
    scene
    script
    timer
    remote
    ;

  turnOn = act "turn_on";
  toggle = entity: act "toggle" entity { };
  fadeOn =
    entity: brightness_pct:
    turnOn entity {
      inherit brightness_pct;
      transition = 300;
    };
  fadeOff = entity: act "turn_off" entity { transition = 300; };
  dim = entity: brightness_step_pct: turnOn entity { inherit brightness_step_pct; };

  sunrise = offset: {
    trigger = "sun";
    event = "sunrise";
    inherit offset;
  };
  sunset = offset: {
    trigger = "sun";
    event = "sunset";
    inherit offset;
  };
  at = time: weekday: {
    trigger = "time";
    at = time;
    inherit weekday;
  };
  timerFinished = timer: {
    trigger = "event";
    event_type = "timer.finished";
    event_data.entity_id = timer.entity_id;
  };

  when = trigger: action: {
    triggers = [ trigger ];
    actions = [ action ];
  };

  # Entities that carry their own config, by domain.
  defined = domain: lib.filter (e: e ? config) (lib.attrValues entities.${domain});
  byId = domain: lib.listToAttrs (map (e: lib.nameValuePair e.id e.config) (defined domain));
in
{
  services.home-assistant.config = {
    automation =
      lib.mapAttrsToList
        (
          alias: a:
          a
          // {
            inherit alias;
            id = slugify alias;
          }
        )
        {
          "Sunset" = when (sunset "-00:05:00") (fadeOn light.porch 100);
          "Sunrise" = when (sunrise "-00:05:00") (fadeOff light.porch);
          "Bed Sunset" = when (sunset "-05:00:00") (fadeOn light.bed 30);
          "Wake Up Alarm" = when (at "07:00:00" [
            "mon"
            "tue"
            "wed"
            "thu"
            "fri"
            "sat"
          ]) (fadeOn light.bed 100);

          "Wake Up Button" = when remote.stairs.config (turnOn scene.morning { });
          "Sleepy Time Button" = when remote.stairs.configDouble (turnOn scene.sleepy { });
          "Dining Room Config Button" = when remote.dining.config (toggle light.window);
          "Buttons 1" = when remote.buttons.one (toggle light.window);
          "Buttons 4" = when remote.buttons.four (toggle switch.espresso);
          "Desk Button 1" = when remote.desk.one (turnOn script.minifridgeSnooze { });
          "Knob Right" = when remote.knob.right (dim light.cans 10);
          "Knob Left" = when remote.knob.left (dim light.cans (-10));

          "Minifridge Wake Up" = when (timerFinished timer.minifridge) (turnOn switch.minifridge { });
        };

    scene = map (e: e.config) (defined "scene");
    light = map (e: e.config) (defined "light");
    script = byId "script";
    timer = byId "timer";
  };
}
