# AP leases are in network.nix, as their MACs are private.
let
  lower = builtins.replaceStrings [ "A" "B" "C" "D" "E" "F" ] [ "a" "b" "c" "d" "e" "f" ];
  audioBoards = builtins.mapAttrs (_: board: {
    inherit (board) address mac;
    clientId = "1:${lower board.mac}";
  }) (import ../hosts/box/home-assistant/esphome/audio-boards.nix);
in
audioBoards
// {
  box = {
    address = "10.0.0.4";
    mac = "B4:2E:99:A1:60:9B";
  };
  fractal = {
    address = "10.0.0.5";
    mac = "9C:6B:00:51:F9:20";
  };
  frame-eth = {
    address = "10.0.0.6";
    mac = "00:E0:4C:A4:5A:50";
  };
  frame-wifi = {
    address = "10.0.0.7";
    mac = "58:CD:C9:3A:85:B9";
  };
  dan-desktop = {
    address = "10.0.0.8";
    mac = "24:4B:FE:5B:1D:01";
  };
  samsung-printer = {
    address = "10.0.0.2";
    mac = "84:25:19:26:B1:09";
    comment = "Samsung Printer";
  };
  elegoo-printer = {
    address = "10.0.0.9";
    mac = "C0:F5:35:8D:D6:30";
    clientId = "ff:35:8d:d6:30:0:1:0:1:2a:71:1a:7f:f2:23:21:ce:82:d8";
    comment = "elegoo printer";
  };
  din-paper = {
    address = "10.0.0.10";
    mac = "44:1B:F6:C1:20:3C";
    clientId = "1:44:1b:f6:c1:20:3c";
  };
  bed-paper = {
    address = "10.0.0.11";
    mac = "44:1B:F6:C1:19:1C";
    clientId = "1:44:1b:f6:c1:19:1c";
  };
}
