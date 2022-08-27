{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpi
    cargo-kcov
    dmidecode
    jc
    kcov
    lshw
    psmisc # killall, fuser, etc.
    sparse
    strace
    usbutils
  ];
}
