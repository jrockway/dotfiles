{
  description = "Home Manager configuration";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-25.11-darwin";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager-linux = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager-darwin = {
      url = "github:nix-community/home-manager/release-25.11";
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
    nix-index-database-linux = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database-darwin = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    uv2nix = {
      url = "github:pyproject-nix/uv2nix";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    truss-src = {
      url = "github:basetenlabs/truss";
      flake = false;
    };
    jlog-src = {
      url = "github:jrockway/json-logs";
      flake = false;
    };
  };
  outputs = { nixpkgs, nixpkgs-unstable, nixpkgs-darwin, flake-utils
    , home-manager-linux, home-manager-darwin, sops-nix-linux, sops-nix-darwin
    , nix-index-database-linux, nix-index-database-darwin, pyproject-nix, uv2nix
    , pyproject-build-systems, truss-src, jlog-src, ... }:
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
        nix-index-database = if isDarwin then
          nix-index-database-darwin
        else
          nix-index-database-linux;
        truss = pkgs.callPackage ./truss {
          inherit pyproject-nix uv2nix pyproject-build-systems truss-src;
        };
        truss-darwin = darwin-pkgs.callPackage ./truss {
          inherit pyproject-nix uv2nix pyproject-build-systems truss-src;
        };
        jlog = pkgs.callPackage ./jlog { inherit jlog-src; };
        jlog-darwin = darwin-pkgs.callPackage ./jlog { inherit jlog-src; };
      in {
        packages.jlog = jlog;
        packages.homeConfigurations."vscode" =
          home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [ ./home.nix nix-index-database.homeModules.default ];
            extraSpecialArgs = {
              username = "vscode";
              inherit unstable;
              inherit sops-nix;
              inherit truss;
              inherit jlog;
            };
          };
        packages.homeConfigurations."jrockway" =
          home-manager.lib.homeManagerConfiguration {
            pkgs = if isDarwin then darwin-pkgs else pkgs;
            modules = [ ./home.nix nix-index-database.homeModules.default ];
            extraSpecialArgs = {
              username = "jrockway";
              inherit unstable;
              inherit sops-nix;
              truss = if isDarwin then truss-darwin else truss;
              jlog = if isDarwin then jlog-darwin else jlog;
            };
          };
      });
}
