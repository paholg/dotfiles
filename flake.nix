{
  description = "Home manager weee";

  # TODO: Add more follows. Maybe a script to detect from flake.lock file?
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.systems.follows = "systems";
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
      url = "github:paholg/helix/driver";

      inputs.nixpkgs.follows = "nixpkgs";
      inputs.crane.follows = "crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    playlister = {
      url = "github:paholg/playlister";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.crane.follows = "crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    # Dependencies to minimize duplicates in `flake.lock`:
    crane = {
      url = "github:ipetkov/crane";
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
        external = {
          agenix = inputs.agenix.packages.${prev.system}.default;
          display-switch = inputs.display-switch.defaultPackage.${prev.system};
          helix = inputs.helix.packages.${prev.system}.default;
          playlister = inputs.playlister.packages.${prev.system}.default;
        };
      };

      pkgs =
        system:
        import inputs.nixpkgs {
          overlays = [
            pkgs_overlay
          ];
          inherit system;
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
                value = ./hosts/${host}/${username}.nix;
              }) config.users
            );
            specialArgs = {
              gui = config.gui;
              linux = true;
              nixos = true;
            };
          in
          inputs.nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            pkgs = pkgs linux;
            system = linux;
            modules = [
              ./nixos
              ./hosts/${host}/nixos.nix
              inputs.agenix.nixosModules.default
              inputs.home-manager.nixosModules.home-manager
              {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.extraSpecialArgs = specialArgs;
                home-manager.users = users;
                home-manager.sharedModules = [
                  inputs.stylix.homeModules.stylix
                  inputs.nix-index-database.homeModules.nix-index
                  {
                    programs.nix-index-database.comma.enable = true;
                  }
                ];
              }
              registry
            ] ++ (if config.gui then [ inputs.niri.nixosModules.niri ] else [ ]);
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
        box = {
          gui = false;
          users = [ "paho" ];
        };
        fractal = {
          gui = true;
          users = [
            "paho"
            "guest"
          ];
        };
        frame = {
          gui = true;
          users = [ "paho" ];
        };
      };
    };
}
