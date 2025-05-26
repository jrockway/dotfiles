{
  description = "Home Manager configuration of jrockway";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { nixpkgs, nixpkgs-unstable, flake-utils, home-manager, sops-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
      in {
        packages.homeConfigurations."jrockway" =
          home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [ ./home.nix ];
            extraSpecialArgs = {
              inherit unstable;
              inherit sops-nix;
            };
          };
      });
}
