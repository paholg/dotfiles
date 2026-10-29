{ lib }:
{
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

  on = brightness: {
    state = "on";
    inherit brightness;
  };

  states = entities: state: lib.listToAttrs (map (e: lib.nameValuePair e.entity_id state) entities);
}
