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
    authit = {
      url = "github:paholg/authit";
    };
    dc = {
      url = "github:paholg/dc";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.crane.follows = "crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    display-switch = {
      url = "github:paholg/display-switch/flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.crane.follows = "crane";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    envswitch = {
      url = "github:paholg/envswitch";
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
      inputs.rust-overlay.follows = "rust-overlay";
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
    rustybar = {
      url = "github:paholg/rustybar/icons";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };
    claude-code = {
      url = "github:sadjow/claude-code-nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
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

      pkgs_overlay = system: final: prev: {
        libtorrent-rakshasa = prev.libtorrent-rakshasa.overrideAttrs (old: rec {
          version = "0.15.6";
          src = prev.fetchFromGitHub {
            owner = "rakshasa";
            repo = "libtorrent";
            rev = "v${version}";
            hash = "sha256-udEe9VyUzPXuCTrB3U3+XCbVWvfTT7xNvJJkLSQrRt4=";
          };
        });
        rtorrent = prev.rtorrent.overrideAttrs (old: rec {
          version = "0.15.6";
          src = prev.fetchFromGitHub {
            owner = "rakshasa";
            repo = "rtorrent";
            rev = "v${version}";
            hash = "sha256-B/5m1JXdUpczUMNN4cy5p6YurjmRFxMQHG3cQFSmZSs=";
          };
        });
        external = {
          agenix = inputs.agenix.packages.${system}.default;
          claude-code = inputs.claude-code.packages.${system}.default;
          dc = inputs.dc.packages.${system}.default;
          display-switch = inputs.display-switch.defaultPackage.${system};
          envswitch = inputs.envswitch.packages.${system}.default;
          helix = inputs.helix.packages.${system}.default;
          playlister = inputs.playlister.packages.${system}.default;
          rustybar = inputs.rustybar.packages.${system}.default;
        };
      };

      pkgs =
        system:
        import inputs.nixpkgs {
          overlays = [
            (pkgs_overlay system)
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
            ]
            ++ (if config.gui then [ inputs.niri.nixosModules.niri ] else [ ])
            ++ (if config.authit or false then [ inputs.authit.nixosModules.default ] else [ ]);
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
          authit = true;
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
