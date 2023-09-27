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
  };

  outputs = inputs:
    with inputs; let
      linux = "x86_64-linux";

      custom_packages = [
        snippets-ls.packages.${linux}.snippets-ls
        helix.packages.${linux}.default
      ];
    in {
      homeConfigurations = {
        "paho@ubuntu" = home-manager.lib.homeManagerConfiguration {
          pkgs =
            import nixpkgs
            {
              system = linux;
              config.allowUnfree = true;
            };
          modules = [
            ./hosts/ubuntu/home.nix
            nur.nixosModules.nur
            {
              home.packages = custom_packages;
            }
          ];
        };
      };
    };
}
