{ config, lib, pkgs, ... }: {
  boot.kernel.sysctl = { "kernel.dmesg_restrict" = 0; };

  environment.systemPackages =
    lib.mkDefault [ pkgs.mg pkgs.gitFull pkgs.home-manager ];

  i18n.defaultLocale = "en_US.UTF-8";

  nix.gc.automatic = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  security.sudo.wheelNeedsPassword = false;

  services.chrony.enable = true;
  services.openssh.enable = true;
  services.prometheus.exporters.node = {
    openFirewall = true;
    enable = true;
  };
  services.tailscale.enable = true;
  services.timesyncd.enable = false; # using chrony instead

  system.autoUpgrade.allowReboot = false;
  system.autoUpgrade.enable = true;
  system.autoUpgrade.flags =
    [ "--update-input" "nixpkgs" "--recreate-lock-file" ];

  time.timeZone = "America/New_York";

  networking.useDHCP = lib.mkDefault true;
}
