{ lib }:
rec {
  # Matches Home Assistant's `slugify` for ASCII names, which it uses to derive
  # entity ids from names.
  slugify =
    name:
    lib.concatStringsSep "_" (
      lib.filter (s: lib.isString s && s != "") (builtins.split "[^a-z0-9]+" (lib.toLower name))
    );

  entity = domain: id: {
    inherit domain id;
    entity_id = "${domain}.${id}";
  };

  act = verb: entity: data: {
    action = "${entity.domain}.${verb}";
    target.entity_id = entity.entity_id;
    inherit data;
  };

  # Dims a light that is brighter than `brightness`; leaves it alone if off or dimmer.
  lowerTo = entity: brightness: {
    "if" = [
      {
        condition = "numeric_state";
        inherit (entity) entity_id;
        attribute = "brightness";
        above = brightness;
      }
    ];
    "then" = [ (act "turn_on" entity { inherit brightness; }) ];
  };

  on = brightness: {
    state = "on";
    inherit brightness;
  };

  states = entities: state: lib.listToAttrs (map (e: lib.nameValuePair e.entity_id state) entities);
}
