# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_6_14;
  boot.kernelModules = [ "panthor" "input_leds" "dw_mmc_rockchip" ];
  boot.initrd.availableKernelModules = lib.mkForce [
    "dw_mmc_rockchip"
    "hid"
    "input_leds"
    "mmc_block"
    "nvme"
    "panthor"
    "sdhci_of_dwcmshc"
  ];

  networking.hostName = "orangepi5max"; # Define your hostname.
  networking.networkmanager.enable = false;
  networking.useDHCP = false;
  systemd.network = {
    enable = true;
    wait-online = {
      timeout = 5;
      anyInterface = true;
    };
    networks."enP3p49s0" = {
      matchConfig.Name = "enP3p49s0";
      networkConfig.DHCP = "ipv4";
      linkConfig.RequiredForOnline = "yes";
    };
  };
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";

  users.users.jrockway = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzAhJaA8i7e1zomE5m+eTrp4GRQpYt0PDkG2RLTouiE"
    ];
  };

  services.openssh.enable = true;
  services.tailscale.enable = true;
  services.prometheus.exporters.node = {
    openFirewall = true;
    enable = true;
  };
  services.chrony.enable = true;
  services.timesyncd.enable = false;

  security.sudo.wheelNeedsPassword = false;
  nix.gc.automatic = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = false;

  environment.systemPackages = with pkgs; [ mg tailscale gitFull home-manager ];

  system.copySystemConfiguration = true;
  system.stateVersion = "25.05"; # Did you read the comment?
}
