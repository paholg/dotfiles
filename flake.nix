{
  description = "Home manager weee";

  # TODO: Add more follows. Maybe a script to detect from flake.lock file?
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # For `command-not-found`:
    flake-programs-sqlite = {
      url = "github:wamserma/flake-programs-sqlite";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    display-switch = {
      url = "github:paholg/display-switch/flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.crane.follows = "crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:paholg/helix/file-picker-navigation";
      # url = "github:helix-editor/helix/master";

      inputs.nixpkgs.follows = "nixpkgs";
      inputs.crane.follows = "crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # ra-multiplex = {
    #   url = "github:paholg/ra-multiplex/temp";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.naersk.follows = "naersk";
    # };
    rustybar = {
      url = "github:paholg/rustybar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.naersk.follows = "naersk";
      inputs.utils.follows = "flake-utils";
    };
    # TODO: Workaround for steam issue.
    # Can remove once 18.1 releases.
    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.unstable.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = "github:xmonad/xmonad-contrib";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.xmonad.follows = "xmonad";
    };

    # Dependencies to minimize duplicates in `flake.lock`:
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    systems = {
      url = "github:nix-systems/default";
    };
  };

  outputs =
    inputs:
    let
      linux = "x86_64-linux";

      pkgs_overlay = final: prev: {
        unfree = import inputs.nixpkgs {
          system = prev.system;
          config.allowUnfree = true;
        };
        anyrun = inputs.anyrun.packages.${prev.system}.anyrun;
        display-switch = inputs.display-switch.defaultPackage.${prev.system};
        helix-custom = inputs.helix.packages.${prev.system}.default;
        # ra-multiplex = ra-multiplex.defaultPackage.${prev.system};
        rustybar = inputs.rustybar.defaultPackage.${prev.system};
      };

      pkgs =
        system:
        import inputs.nixpkgs {
          overlays = [
            pkgs_overlay
            inputs.xmonad.overlay
            inputs.xmonad-contrib.overlay
          ];
          inherit system;
          # FIXME
          config.allowUnfree = true;
        };

      # use nix flake inputs for registry, for e.g. search.
      registry =
        { lib, ... }:
        {
          nix.registry = lib.mapAttrs (_: flake: { inherit flake; }) inputs;
        };

      nixos =
        hosts:
        builtins.mapAttrs (
          host: config:
          let
            users = builtins.listToAttrs (
              map (username: {
                name = username;
                value = import ./hosts/${host}/${username}.nix;
              }) config.users
            );
          in
          inputs.nixpkgs.lib.nixosSystem {
            pkgs = pkgs linux;
            system = linux;
            modules = [
              ./hosts/${host}/nixos.nix
              ./nixos
              inputs.home-manager.nixosModules.home-manager
              # For `command-not-found`:
              inputs.flake-programs-sqlite.nixosModules.programs-sqlite
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users = users;
              }
              registry
            ];
          }
        ) hosts;
    in
    {
      homeConfigurations = {
        "paho@ubuntu" = inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs linux;
          modules = [
            ./hosts/ubuntu/paho.nix
            registry
          ];
        };
      };

      nixosConfigurations = nixos {
        box.users = [ "paho" ];
        fractal.users = [
          "paho"
          "guest"
        ];
        t14s.users = [ "paho" ];
      };
    };
}
