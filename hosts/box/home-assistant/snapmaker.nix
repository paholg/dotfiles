# Snapmaker U1 (Klipper/Moonraker) print notifications.
#
# Not using the moonraker HA integration, as we just want notifications, not the
# ability to toggle heaters, etc.
#
# https://moonraker.readthedocs.io/en/latest/external_api/printer/#query-printer-object-status
# https://www.home-assistant.io/integrations/rest/
{ ... }:
let
  moonraker = "http://10.0.0.15:7125";
  printer = "snapmaker_u1";
  printState = "sensor.${printer}_print_state";
  person = "person.paho_lurie_gregg_2";

  # Notify every companion-app phone attached to `person`. The app adds its
  # device_tracker to the logged-in user's person on registration, so new phones
  # are picked up automatically. Each phone's notify action is
  # `notify.mobile_app_<slugified device name>`.
  # https://companion.home-assistant.io/docs/notifications/notifications-basic/
  notifyPhones = data: {
    repeat = {
      for_each = ''
        {% set phones = integration_entities('mobile_app') | map('device_id') | list %}
        {{ state_attr('${person}', 'device_trackers')
           | map('device_id') | select('in', phones)
           | map('device_attr', 'name') | map('slugify') | unique | list }}'';
      sequence = [
        {
          action = "notify.mobile_app_{{ repeat.item }}";
          inherit data;
        }
      ];
    };
  };
in
{
  services.home-assistant = {
    config.rest = [
      {
        resource = "${moonraker}/printer/objects/query?print_stats";
        scan_interval = 60;
        sensor = [
          {
            name = "Snapmaker U1 Print State";
            unique_id = "${printer}_print_state";
            value_template = "{{ value_json.result.status.print_stats.state }}";
          }
          {
            name = "Snapmaker U1 Filename";
            unique_id = "${printer}_filename";
            value_template = "{{ value_json.result.status.print_stats.filename }}";
          }
          {
            name = "Snapmaker U1 Print Message";
            unique_id = "${printer}_print_message";
            value_template = "{{ value_json.result.status.print_stats.message }}";
          }
        ];
      }
    ];

    config.automation = [
      {
        alias = "Snapmaker Print Finished";
        id = "snapmaker_print_finished";
        triggers = [
          {
            trigger = "state";
            entity_id = printState;
            from = [
              "printing"
              "paused"
            ];
            to = [
              "complete"
              "cancelled"
              "error"
            ];
          }
        ];
        actions = [
          (notifyPhones {
            title = ''
              {{ {'complete': '✅ Print complete',
                  'cancelled': '🛑 Print cancelled',
                  'error': '❌ Print failed'}[trigger.to_state.state] }}'';
            message = "{{ states('sensor.${printer}_filename') }}{% if trigger.to_state.state == 'error' %}: {{ states('sensor.${printer}_print_message') }}{% endif %}";
            data = {
              tag = "snapmaker-print";
              clickAction = "https://snapmaker.paholg.com";
              priority = "high";
              channel = "Printer";
            };
          })
        ];
      }
    ];
  };
}
