{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      sops-nix,
      ...
    }@inputs:
    {
      nixosConfigurations = {
        nixos-vm = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            sops-nix.nixosModules.sops
            ./base.nix
            ./jrockway.nix
            ./jrockway-password.nix
            ./hosts/nixos-vm.nix
          ];
        };

        # Not an image. Launch the official NixOS AMI with ./aws-user-data.sh as
        # user-data and first boot switches to this; see hosts/aws.nix.
        aws = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (nixpkgs + "/nixos/modules/virtualisation/amazon-image.nix")
            sops-nix.nixosModules.sops
            ./base.nix
            ./jrockway.nix
            ./hosts/aws.nix
          ];
        };

        # nom build .#nixosConfigurations.rk3588-installer.config.system.build.isoImage
        rk3588-installer = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (nixpkgs + "/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
            ./rk3588.nix
            ./iso.nix
            ./installer.nix
          ];
        };

        berry-rock5bplus-0 = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (nixpkgs + "/nixos/modules/installer/scan/not-detected.nix")
            sops-nix.nixosModules.sops
            ./base.nix
            ./no-networkmanager.nix
            ./rk3588.nix
            ./rock5b-plus.nix
            ./rock5b-plus-k8s-node-fs.nix
            ./berry-cluster.nix
            ./jrockway.nix
            ./jrockway-password.nix
            ./hosts/berry-rock5bplus-0.nix
          ];
        };

        berry-rock5bplus-1 = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (nixpkgs + "/nixos/modules/installer/scan/not-detected.nix")
            sops-nix.nixosModules.sops
            ./base.nix
            ./no-networkmanager.nix
            ./rk3588.nix
            ./rock5b-plus.nix
            ./rock5b-plus-k8s-node-fs.nix
            ./berry-cluster.nix
            ./jrockway.nix
            ./jrockway-password.nix
            ./hosts/berry-rock5bplus-1.nix
          ];
        };

        berry-rock5bplus-2 = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (nixpkgs + "/nixos/modules/installer/scan/not-detected.nix")
            sops-nix.nixosModules.sops
            ./base.nix
            ./no-networkmanager.nix
            ./rk3588.nix
            ./rock5b-plus.nix
            ./rock5b-plus-k8s-node-fs.nix
            ./berry-cluster.nix
            ./jrockway.nix
            ./jrockway-password.nix
            ./hosts/berry-rock5bplus-2.nix
          ];
        };

        orangepi5max = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            (nixpkgs + "/nixos/modules/installer/scan/not-detected.nix")
            sops-nix.nixosModules.sops
            ./base.nix
            ./no-networkmanager.nix
            ./rk3588.nix
            ./orangepi5-max.nix
            ./jrockway.nix
            ./jrockway-password.nix
            ./hosts/orangepi5max.nix
          ];
        };
      };
    };
}
