{
  description = "Home manager weee";

  # TODO: Add more follows. Maybe a script to detect from flake.lock file?
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:paholg/helix/temp";
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
        helix = helix.packages.${prev.system}.default;
        ra-multiplex = ra-multiplex.defaultPackage.${prev.system};
        snippets-ls = snippets-ls.packages.${prev.system}.snippets-ls;
        rustybar = rustybar.defaultPackage.${prev.system};
      };

      pkgs = system:
        import nixpkgs {
          overlays = [pkgs_overlay];
          inherit system;
        };
    in {
      homeConfigurations = {
        "paho@ubuntu" = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs linux;
          modules = [
            ./hosts/ubuntu/home.nix
            nur.nixosModules.nur
          ];
        };

        "paho@box" = home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs linux;
          modules = [./hosts/box/home.nix];
        };
      };
    };
}
