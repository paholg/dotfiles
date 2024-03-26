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
      # url = "github:paholg/helix/temp";
      # url = "github:helix-editor/helix/master";

      inputs.nixpkgs.follows = "nixpkgs";
    };
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ra-multiplex = {
      url = "github:paholg/ra-multiplex/temp";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.naersk.follows = "naersk";
    };
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
        ra-multiplex = ra-multiplex.defaultPackage.${prev.system};
        snippets-ls = snippets-ls.packages.${prev.system}.snippets-ls;
        rustybar = rustybar.defaultPackage.${prev.system};
      };

      pkgs = system:
        import nixpkgs {
          overlays = [pkgs_overlay];
          inherit system;
          # FIXME
          config.allowUnfree = true;
        };
    in {
      homeConfigurations = {
        "paho@ubuntu" = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs linux;
          modules = [
            ./hosts/ubuntu/home.nix
            nur.nixosModules.nur
            ({lib, ...}: {
              nix.registry = lib.mapAttrs (_: flake: {inherit flake;}) inputs;
            })
          ];
        };

        "paho@box" = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs linux;
          modules = [
            ./hosts/box/home.nix
          ];
        };

        # "paho@fractal" = home-manager.lib.homeManagerConfiguration {
        #   pkgs = pkgs linux;
        #   modules = [
        #     ./hosts/fractal/home.nix
        #     nur.nixosModules.nur
        #   ];
        # };
      };

      nixosConfigurations = {
        box = nixpkgs.lib.nixosSystem {
          pkgs = pkgs linux;
          system = linux;
          modules = [
            ./hosts/box/configuration.nix
            # For `command-not-found`:
            inputs.flake-programs-sqlite.nixosModules.programs-sqlite
            ({lib, ...}: {
              nix.registry = lib.mapAttrs (_: flake: {inherit flake;}) inputs;
            })
          ];
        };

        fractal = nixpkgs.lib.nixosSystem {
          pkgs = pkgs linux;
          system = linux;
          modules = [
            ./hosts/fractal/configuration.nix
            home-manager.nixosModules.home-manager
            # nur.nixosModules.nur

            # For `command-not-found`:
            inputs.flake-programs-sqlite.nixosModules.programs-sqlite
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.paho = import ./hosts/fractal/home.nix;
            }
            ({lib, ...}: {
              nix.registry = lib.mapAttrs (_: flake: {inherit flake;}) inputs;
            })
          ];
        };
      };
    };
}
