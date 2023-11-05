{...}: {
  imports = [../../nix/common.nix ../../nix/gui.nix ../../nix/ssh.nix];

  networking.hostName = "fractal";
}
