# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];
  system.stateVersion = "24.11";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos-vm";
  networking.networkmanager.enable = true;
  networking.interfaces.eth0 = {
    useDHCP = false;
    ipv4.addresses = [{
      address = "192.168.254.4";
      prefixLength = 24;
    }];
  };

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";

  users.users.jrockway = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    # packages = with pkgs; [
    # ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzAhJaA8i7e1zomE5m+eTrp4GRQpYt0PDkG2RLTouiE"
    ];
  };

  environment.systemPackages = with pkgs; [ mg tailscale gitFull home-manager ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # Services
  services.openssh.enable = true;
  services.tailscale.enable = true;

  services.prometheus.exporters.node = {
    openFirewall = true;
    enable = true;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  system.copySystemConfiguration = true;

  # programs
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ pkgs.glibc ];

  # security
  security.sudo.wheelNeedsPassword = false;
}
