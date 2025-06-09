{ config, lib, pkgs, ... }: {
  networking.hostName = "nixos-vm";

  nixpkgs.hostPlatform = "x86_64-linux";

  system.stateVersion = "24.11";

  boot.initrd.availableKernelModules = [ "sd_mod" "sr_mod" ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/639862cb-5197-4aea-92f0-7938a0e248a9";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3804-FE66";
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  virtualisation.hypervGuest.enable = true;

  networking.networkmanager.enable = true;
  networking.interfaces.eth0 = {
    useDHCP = false;
    ipv4.addresses = [{
      address = "192.168.254.4";
      prefixLength = 24;
    }];
  };

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ glibc ];
}
