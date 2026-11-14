# Snapmaker U1 (Klipper/Moonraker) print and calibration notifications.
#
# Not using the moonraker HA integration, as we just want notifications, not the
# ability to toggle heaters, etc.
#
# https://moonraker.readthedocs.io/en/latest/external_api/printer/#query-printer-object-status
# https://github.com/Snapmaker/u1-klipper/blob/main/klippy/extras/machine_state_manager.py
# https://www.home-assistant.io/integrations/rest/
{ ... }:
let
  moonraker = "http://10.0.0.15:7125";
  printer = "snapmaker_u1";
  printState = "sensor.${printer}_print_state";
  activity = "sensor.${printer}_activity";
  actionCode = "sensor.${printer}_action_code";
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
        resource = "${moonraker}/printer/objects/query?print_stats&exception_manager&machine_state_manager";
        scan_interval = 60;
        sensor = [
          {
            name = "Snapmaker U1 Print State";
            unique_id = "${printer}_print_state";
            value_template = "{{ value_json.result.status.print_stats.state }}";
          }
          {
            name = "Snapmaker U1 Print Filename";
            unique_id = "${printer}_print_filename";
            value_template = "{{ value_json.result.status.print_stats.filename }}";
          }
          {
            name = "Snapmaker U1 Print Message";
            unique_id = "${printer}_print_message";
            value_template = "{{ value_json.result.status.print_stats.message }}";
          }
          {
            # Snapmaker firmware extension: why the printer paused itself, e.g.
            # "detected noodle" for the camera's spaghetti detection.
            name = "Snapmaker U1 Exception";
            unique_id = "${printer}_exception";
            value_template = ''
              {% set s = value_json.result.status %}
              {{ ([s.print_stats.exception.message]
                  + (s.exception_manager.exceptions | map(attribute='message') | list))
                 | select('string') | select | unique | join(', ') }}'';
          }
          {
            # Snapmaker firmware extension: what the machine as a whole is
            # doing (MachineMainState), which covers calibrations and other
            # tasks that never touch print_stats.
            name = "Snapmaker U1 Activity";
            unique_id = "${printer}_activity";
            value_template = ''
              {% set s = value_json.result.status.machine_state_manager.main_state %}
              {{ {0: 'Idle',
                  1: 'Printing',
                  2: 'XYZ offset calibration',
                  3: 'Bed leveling',
                  4: 'Flow calibration',
                  5: 'Input shaper calibration',
                  6: 'Firmware upgrade',
                  7: 'Abnormal',
                  8: 'Bed screw adjustment',
                  9: 'Filament load',
                  10: 'Filament unload',
                  11: 'Manual filament load',
                  12: 'Park point calibration',
                  13: 'Homing origin calibration'}.get(s, s) }}'';
          }
          {
            # The step within the current activity (ActionCode).
            name = "Snapmaker U1 Action Code";
            unique_id = "${printer}_action_code";
            value_template = "{{ value_json.result.status.machine_state_manager.action_code }}";
          }
        ];
      }
    ];

    config.automation = [
      {
        alias = "Snapmaker Print Stopped";
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
              "paused"
              "complete"
              "cancelled"
              "error"
            ];
          }
        ];
        actions = [
          (notifyPhones {
            title = ''
              {{ {'paused': '⏸️ Print paused',
                  'complete': '✅ Print complete',
                  'cancelled': '🛑 Print cancelled',
                  'error': '❌ Print failed'}[trigger.to_state.state] }}'';
            message = ''
              {% set sensor = {'paused': 'exception', 'error': 'print_message'}.get(trigger.to_state.state) %}
              {% set reason = states('sensor.${printer}_' ~ sensor) if sensor else none %}
              {{ states('sensor.${printer}_print_filename') }}
              {%- if reason and reason not in ['unknown', 'unavailable'] %}: {{ reason }}{% endif %}'';
            data = {
              tag = "snapmaker-print";
              clickAction = "https://u1.paholg.com";
              priority = "high";
              channel = "Printer";
            };
          })
        ];
      }
      {
        alias = "Snapmaker Needs Attention";
        id = "snapmaker_needs_attention";
        triggers = [
          {
            # A standalone task ended. Prints are handled above, and filament
            # loading is quick enough that we're standing at the printer.
            id = "done";
            trigger = "state";
            entity_id = activity;
            from = [
              "XYZ offset calibration"
              "Bed leveling"
              "Flow calibration"
              "Input shaper calibration"
              "Firmware upgrade"
              "Bed screw adjustment"
              "Park point calibration"
              "Homing origin calibration"
            ];
            to = "Idle";
          }
          {
            id = "abnormal";
            trigger = "state";
            entity_id = activity;
            not_from = [
              "unknown"
              "unavailable"
            ];
            to = "Abnormal";
          }
          {
            # Steps where the printer waits on us: MANUAL_CLEAN_EXTRUDER{,1,2,3}
            # and MANUAL_TUNING.
            id = "action";
            trigger = "state";
            entity_id = actionCode;
            to = [
              "192"
              "193"
              "194"
              "195"
              "514"
            ];
          }
        ];
        actions = [
          (notifyPhones {
            title = ''
              {% set exception = states('sensor.${printer}_exception') not in ['unknown', 'unavailable', ""] %}
              {% if trigger.id == 'action' %}🔧 Printer is waiting on you
              {% elif trigger.id == 'abnormal' %}⚠️ Printer needs attention
              {% elif exception %}⚠️ {{ trigger.from_state.state }} stopped
              {% else %}✅ {{ trigger.from_state.state }} finished
              {% endif %}'';
            message = ''
              {% set exception = states('sensor.${printer}_exception') %}
              {% if trigger.id == 'action' %}
                {{ {'192': 'Clean nozzle 1',
                    '193': 'Clean nozzle 2',
                    '194': 'Clean nozzle 3',
                    '195': 'Clean nozzle 4',
                    '514': 'Adjust the bed screws'}[trigger.to_state.state] }}
              {% elif exception not in ['unknown', 'unavailable', ""] %}{{ exception }}
              {% elif trigger.id == 'abnormal' %}During: {{ trigger.from_state.state | lower }}
              {% else %}Ready for the next step
              {% endif %}'';
            data = {
              tag = "snapmaker-task";
              clickAction = "https://u1.paholg.com";
              priority = "high";
              channel = "Printer";
            };
          })
        ];
      }
    ];
  };
}
