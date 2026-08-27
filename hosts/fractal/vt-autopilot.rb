# Switch VTs between the guest TV session (tty1) and the desktop (tty2).
#
# Rule A: tty1 active and no input for IDLE_SECONDS -> chvt 2.
# Rule B: gamepad button press while tty2 is active -> chvt 1, but only if paho
#         is locked or not logged in.

IDLE_SECONDS = 10 * 60
COOLDOWN_SECONDS = 5
# Fallback only; inotify triggers rescans as soon as devices change.
RESCAN_SECONDS = 60

# See linux/input-event-codes.h.
EV_KEY = 1
EV_REL = 2
BTN_GAMEPAD = 0x130
# struct input_event: 16-byte timeval, u16 type, u16 code, s32 value.
EVENT_SIZE = 24

def monotonic
  Process.clock_gettime(Process::CLOCK_MONOTONIC)
end

# Watch /dev/input for a newly connected controller; events from before a device
# is opened are never delivered.
def inotify_watch(dir)
  require "fiddle"
  libc = Fiddle.dlopen(nil)
  init = Fiddle::Function.new(
    libc["inotify_init1"], [Fiddle::TYPE_INT], Fiddle::TYPE_INT
  )
  add_watch = Fiddle::Function.new(
    libc["inotify_add_watch"],
    [Fiddle::TYPE_INT, Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT],
    Fiddle::TYPE_INT
  )
  in_nonblock = 0o4000
  in_create = 0x100
  in_delete = 0x200
  fd = init.call(in_nonblock)
  raise "inotify_init1 failed" if fd.negative?
  raise "inotify_add_watch failed" if add_watch.call(fd, dir, in_create | in_delete).negative?

  IO.for_fd(fd)
end

def active_vt
  File.read("/sys/class/tty/tty0/active").strip
end

def pgrep(user, pattern)
  # Regex match against the process name. Note nix wraps some binaries,
  # e.g. swaylock's process is ".swaylock-wrapped".
  system("pgrep", "-u", user, pattern, out: File::NULL)
end

def paho_interruptible?
  # swaylock does not set logind's LockedHint, so check processes instead.
  pgrep("paho", "swaylock") || !pgrep("paho", "^niri$")
end

def chvt(vt)
  puts "switching to tty#{vt}"
  $stdout.flush
  system("chvt", vt.to_s)
end

def gamepad?(path)
  # Sysfs exposes EV_KEY capabilities as space-separated hex words, most
  # significant first.
  caps = File.read("/sys/class/input/#{File.basename(path)}/device/capabilities/key")
  bits = caps.split.reduce(0) { |acc, word| (acc << 64) | word.to_i(16) }
  bits[BTN_GAMEPAD] == 1
rescue SystemCallError
  false
end

def rescan(devices)
  paths = Dir.glob("/dev/input/event*")
  devices.select! do |io, info|
    # A reconnecting device (e.g. a controller powering on) is often
    # recreated under the same path; compare device numbers to catch the
    # stale fd, or its replacement is never opened.
    keep = paths.include?(info[:path]) &&
           begin
             io.stat.rdev == File.stat(info[:path]).rdev
           rescue SystemCallError
             false
           end
    io.close unless keep
    keep
  end
  known = devices.map { |_io, info| info[:path] }
  (paths - known).each do |path|
    begin
      io = File.open(path, "rb")
    rescue SystemCallError
      next
    end
    gp = gamepad?(path)
    puts "tracking #{path} gamepad=#{gp}" if gp
    $stdout.flush
    devices[io] = { path: path, gamepad: gp }
  end
end

devices = {}
inotify = inotify_watch("/dev/input")
rescan(devices)
last_scan = monotonic
last_activity = monotonic
last_switch = 0.0

loop do
  # One wake per second regardless of event rate: the kernel buffers events
  # and we drain them in a single batch. Overflow drops old events
  # (SYN_DROPPED), which we don't care about. Device hotplug wakes the wait
  # immediately via inotify.
  hotplug, = IO.select([inotify], nil, nil, 1.0)
  now = monotonic
  if hotplug
    begin
      loop { inotify.read_nonblock(4096) }
    rescue IO::WaitReadable
      # drained
    end
    rescan(devices)
    last_scan = now
  elsif now - last_scan > RESCAN_SECONDS
    rescan(devices)
    last_scan = now
  end

  gamepad_press = false
  dead = []
  devices.each do |io, info|
    loop do
      begin
        data = io.read_nonblock(EVENT_SIZE * 512)
      rescue IO::WaitReadable
        break
      rescue SystemCallError, EOFError
        dead << io
        break
      end
      (data.size / EVENT_SIZE).times do |i|
        type, _code, value = data[i * EVENT_SIZE, EVENT_SIZE].unpack("x16S2l")
        # EV_ABS is ignored: analog stick drift emits constant noise that
        # would defeat the idle timer.
        if type == EV_REL
          last_activity = now
        elsif type == EV_KEY && value == 1
          last_activity = now
          gamepad_press = true if info[:gamepad]
        end
      end
    end
  end
  dead.each do |io|
    io.close
    devices.delete(io)
    # A dead fd may mean its path was already recreated; rescan next tick.
    last_scan = 0.0
  end

  next if now - last_switch < COOLDOWN_SECONDS

  vt = active_vt
  if vt == "tty1" && now - last_activity > IDLE_SECONDS
    chvt(2)
    last_switch = now
    last_activity = now
  elsif vt == "tty2" && gamepad_press && paho_interruptible?
    chvt(1)
    last_switch = now
    last_activity = now
  end
end
