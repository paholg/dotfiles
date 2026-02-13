busctl --user monitor --json=short \
  --match="eavesdrop=true,type='method_call',interface='org.freedesktop.Notifications',member='Notify'" |
while read -r line; do
  sender=$(echo "$line" | jq -r '.sender')
  sender_pid=$(busctl --user --json=short call \
    org.freedesktop.DBus /org/freedesktop/DBus \
    org.freedesktop.DBus GetConnectionUnixProcessID s "$sender" < /dev/null \
    | jq -r '.data[0]')

  windows=$(niri msg --json windows < /dev/null)

  pid=$sender_pid
  while [ "$pid" -gt 1 ] 2>/dev/null; do
    match=$(echo "$windows" | jq -r \
      '[.[] | select(.pid == '"$pid"')][0].id // empty')
    if [ -n "$match" ]; then
      niri msg action set-window-urgent --id "$match" < /dev/null
      break
    fi
    pid=$(awk '{print $4}' "/proc/$pid/stat" 2>/dev/null) || break
  done
done
