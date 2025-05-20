{
  description = "Home Manager configuration of jrockway";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager-linux = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-darwin = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
    sops-nix-linux = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix-darwin = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
  };
  outputs = { nixpkgs, nixpkgs-unstable, nixpkgs-darwin, flake-utils
    , home-manager-linux, home-manager-darwin, sops-nix-linux, sops-nix-darwin
    , ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        isDarwin = pkgs.stdenv.isDarwin;
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
        darwin-pkgs = import nixpkgs-darwin {
          inherit system;
          config.allowUnfree = true;
        };
        home-manager =
          if isDarwin then home-manager-darwin else home-manager-linux;
        sops-nix = if isDarwin then sops-nix-darwin else sops-nix-linux;
      in {
        packages.homeConfigurations."jrockway" =
          home-manager.lib.homeManagerConfiguration {
            pkgs = if isDarwin then darwin-pkgs else pkgs;
            modules = [ ./home.nix ];
            extraSpecialArgs = {
              inherit unstable;
              inherit sops-nix;
            };
          };
      });
}
