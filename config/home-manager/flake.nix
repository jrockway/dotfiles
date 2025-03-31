{
  # Install Nix first:
  # linux: $ sh <(curl -L https://nixos.org/nix/install) --daemon
  # mac:   $ sh <(curl -L https://nixos.org/nix/install)
  #
  # Then link config/home-manager and config/nix into ~/.config.
  #
  # Finally,
  # $ nix run home-manager/release-24.11 -- init --switch
  description = "Home Manager configuration of jrockway";
  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nixpkgs-unstable, flake-utils, home-manager, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        unstable = nixpkgs-unstable.legacyPackages.${system};
      in {
        packages.homeConfigurations."jrockway" =
          home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [ ./home.nix ];
            extraSpecialArgs = {
              inherit unstable;
              inherit system;
            };
          };
      });
}
