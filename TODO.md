# Temporary workarounds

Things that exist only until something upstream changes. Each entry says where
it lives and what has to happen before it can go. Statuses were last checked on
2026-11-10.

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

- **playlister** — `hosts/box/nixos.nix`, import commented out until playlister
  uses Tidal's modern API.
- **wireguard** — `hosts/box/nixos.nix`, import commented out, "get working".
