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
    python-language-server
    sparse
    strace
    usbutils
  ];
}
