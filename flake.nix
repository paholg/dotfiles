{
  description = "Home manager weee";

  # TODO: Add more follows. Maybe a script to detect from flake.lock file?
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    # For `command-not-found`:
    flake-programs-sqlite = {
      url = "github:wamserma/flake-programs-sqlite";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    anyrun = {
      url = "github:Kirottu/anyrun";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    display-switch = {
      url = "github:paholg/display-switch/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:paholg/helix/file-picker-navigation";
      # url = "github:helix-editor/helix/master";

      inputs.nixpkgs.follows = "nixpkgs";
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
    };
  };

  outputs = inputs:
    with inputs; let
      linux = "x86_64-linux";

      pkgs_overlay = final: prev: {
        unfree = import nixpkgs {
          system = prev.system;
          config.allowUnfree = true;
        };
        anyrun = anyrun.packages.${prev.system}.anyrun;
        display-switch = display-switch.defaultPackage.${prev.system};
        helix = helix.packages.${prev.system}.default;
        # ra-multiplex = ra-multiplex.defaultPackage.${prev.system};
        rustybar = rustybar.defaultPackage.${prev.system};
      };

      pkgs = system:
        import nixpkgs {
          overlays = [pkgs_overlay];
          inherit system;
          # FIXME
          config.allowUnfree = true;
        };

      # use nix flake inputs for registry, for e.g. search.
      registry = {lib, ...}: {
        nix.registry = lib.mapAttrs (_: flake: {inherit flake;}) inputs;
      };

      nixos = hosts:
        builtins.mapAttrs (host: config: let
          username = config.username or "paho";
          modules = config.modules or [];
        in
          nixpkgs.lib.nixosSystem {
            pkgs = pkgs linux;
            system = linux;
            modules =
              [
                ./hosts/${host}/configuration.nix
                home-manager.nixosModules.home-manager
                # For `command-not-found`:
                inputs.flake-programs-sqlite.nixosModules.programs-sqlite
                {
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${username}" = import ./hosts/${host}/home.nix;
                }
                registry
              ]
              ++ modules;
          })
        hosts;
    in {
      homeConfigurations = {
        "paho@ubuntu" = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs linux;
          modules = [
            ./hosts/ubuntu/home.nix
            nur.nixosModules.nur
            registry
          ];
        };
      };

      nixosConfigurations = nixos {
        box = {};
        fractal = {};
      };
    };
}
