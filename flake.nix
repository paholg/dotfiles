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
    devconcurrent = {
      url = "github:paholg/devconcurrent";
    };
    display-switch = {
      url = "github:haimgel/display-switch";
      flake = false;
    };
    envswitch = {
      url = "github:paholg/envswitch";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:paholg/helix/driver";
    };
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    playlister = {
      url = "github:paholg/playlister";
    };
    rustybar = {
      url = "github:paholg/rustybar/icons";
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
    systems = {
      url = "github:nix-systems/default";
    };
  };

  outputs =
    inputs:
    let
      linux = "x86_64-linux";

      pkgs_overlay = system: final: prev: {
        # NOTE: This is likely fixed once a version above 0.16.6 releases.
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
          devconcurrent = inputs.devconcurrent.packages.${system}.default;
          display-switch =
            let
              craneLib = inputs.crane.mkLib final;
            in
            craneLib.buildPackage {
              src = craneLib.cleanCargoSource inputs.display-switch;
              nativeBuildInputs = [ final.pkg-config ];
              buildInputs = [ final.udev final.libxi ];
              doCheck = false;
            };
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
