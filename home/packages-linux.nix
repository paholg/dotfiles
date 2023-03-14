{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpi
    cargo-kcov
    dmidecode
    jc
    kcov
    lshw
    outils # sha256, etc.
    psmisc # killall, fuser, etc.
    sparse
    strace
    usbutils
  ];
}
