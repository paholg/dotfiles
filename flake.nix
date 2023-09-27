{
  description = "Home manager weee";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    snippets-ls = {
      url = "/home/paho/git/snippets-ls";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:paholg/helix/temp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # ra-multiplex = {
    #   url = "github:paholg/ra-multiplex/min_available_memory";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };

  outputs = inputs:
    with inputs; let
      linux = "x86_64-linux";

      pkgs_overlay = final: prev: {
        helix = helix.packages.${prev.system}.default;
        # ra-multiplex = ra-multiplex.packages.${prev.system}.default;
        snippets-ls = snippets-ls.packages.${prev.system}.snippets-ls;
      };
    in {
      homeConfigurations = {
        "paho@ubuntu" = home-manager.lib.homeManagerConfiguration {
          pkgs =
            import nixpkgs
            {
              overlays = [pkgs_overlay];
              system = linux;
              config.allowUnfree = true;
            };
          modules = [
            ./hosts/ubuntu/home.nix
            nur.nixosModules.nur
          ];
        };

        "paho@box" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {system = linux;};
          modules = [./hosts/box/home.nix];
        };
      };
    };
}
