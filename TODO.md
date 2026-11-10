# Temporary workarounds

Things that exist only until something upstream changes. Each entry says where
it lives and what has to happen before it can go. Statuses were last checked on
2026-11-10.

## Ready to remove

- **esphome from a nixpkgs PR** — `flake.nix` (`nixpkgs-esphome` input and
  `packages.esphome`). Pinned to
  [nixpkgs#581242](https://github.com/NixOS/nixpkgs/pull/581242) for
  SendspinZero, which needs 2026.10.0 or later. The PR merged on 2026-11-03 and
  our `nixpkgs` input already has 2026.10.1, so the extra input can be dropped
  and `esphome` taken from `pkgs`.
- **kitty `auto_reload_config = -1`** — `home/gui/kitty.nix`. Works around
  kitty watching the whole nix store; the comment says it is fixed after
  0.47.1, and nixpkgs now has 0.50.0. Remove the setting and confirm kitty
  behaves.

## Probably removable, needs testing

- **xwayland-satellite pinned to 0.8** — `home/gui/niri/default.nix`. 0.8.2
  closed Steam's menus the instant they opened
  ([xwayland-satellite#503](https://github.com/Supreeeme/xwayland-satellite/issues/503)).
  The issue was closed on 2026-09-16, 0.8.3 was released on 2026-09-24, and
  nixpkgs has 0.8.3. Drop the override and check Steam's menus.
- **linux-firmware pinned to 20260622** — `hosts/fractal/nixos.nix`. Later
  GC 11.0.0 firmware hung Navi31 with "Illegal opcode in command stream"
  ([vkd3d-proton#3004](https://github.com/HansKristian-Work/vkd3d-proton/issues/3004));
  AMD reverted it on 2026-09-18. nixpkgs now has 20261019, which postdates the
  revert. Drop the overlay and check for GPU resets in DX12 games.

## Waiting on upstream

- **orca-slicer 2.5.0 as a wrapped AppImage** — `home/packages.nix`. nixpkgs is
  on 2.4.2, and building 2.5.0 from source needs dependencies that aren't
  packaged (wxInspector, SolveSpaceLib, an embedded CPython 3.12.13). Remove
  the `orca-slicer` binding once nixpkgs ships 2.5.0; track
  [nixpkgs#585538](https://github.com/NixOS/nixpkgs/issues/585538).
- **box pinned to the 6.12 LTS kernel** — `hosts/box/nixos.nix`. 6.18 hit a ZFS
  LZ4 decompression oops on this host in May 2026. The default kernel is now
  6.18.62; there is no upstream issue linked, so this needs a deliberate retest
  or a decision to stay on LTS for good.
- **cookit pinned to a commit** — `flake.nix`. "New versions break
  everything"; unpin once cookit is fixed or the config is migrated.

## Disabled until fixed

- **blender-hip** — `hosts/fractal/paho.nix`, commented out as broken. The
  attribute has since been removed from nixpkgs in favour of
  `pkgsRocm.blender` (or `config.rocmSupport = true`), so the comment needs
  updating either way.
- **playlister** — `hosts/box/nixos.nix`, import commented out until playlister
  uses Tidal's modern API.
- **wireguard** — `hosts/box/nixos.nix`, import commented out, "get working".

## Workarounds with no exit in sight

Listed so they aren't forgotten, but nothing upstream is expected to change.

- **tidal-hifi `--no-sandbox` wrapper** — `home/packages.nix`.
  [tidal-hifi#958](https://github.com/Mastermindzh/tidal-hifi/issues/958) was
  closed with the flag as the documented answer, not a fix.
- **Steam `--unsetenv TZ`** — `hosts/fractal/nixos.nix`, for Proton games using
  the wrong timezone
  ([nixpkgs#338266](https://github.com/NixOS/nixpkgs/issues/338266), closed in
  2024 without removing the need for the override, as far as the config
  shows).
- **frame: Panel Self Refresh disabled** — `hosts/frame/nixos.nix`
  (`amdgpu.dcdebugmask=0x10`), for DMCUB / flip_done freezes on Phoenix. Worth
  retrying after a kernel or BIOS update.
- **kanidm `/var/lib/kanidm` bind mount** — `hosts/box/auth.nix`. The module
  won't let `db_path` be set;
  [nixpkgs#407768](https://github.com/NixOS/nixpkgs/pull/407768), which would
  have allowed it, was closed unmerged.
- **Insta360 Link 2 autosuspend udev rule** — `nixos/gui/default.nix`. Could go
  if the kernel gains the same quirk it has for the Link 1.
