{ config, lib, pkgs, ... }: {
  boot.kernel.sysctl = { "kernel.dmesg_restrict" = 0; };

  environment.systemPackages =
    [ pkgs.mg pkgs.gitFull pkgs.home-manager pkgs.smartmontools ];

  i18n.defaultLocale = "en_US.UTF-8";

  nix.gc.automatic = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  security.sudo.wheelNeedsPassword = false;

  services.chrony.enable = true;
  services.openssh.enable = true;
  services.prometheus.exporters.chrony = {
    enable = true;
    openFirewall = true;
  };
  services.prometheus.exporters.node = {
    enable = true;
    openFirewall = true;
  };
  services.tailscale.enable = true;
  services.timesyncd.enable = false; # using chrony instead

  system.autoUpgrade.allowReboot = false;
  system.autoUpgrade.enable = true;
  system.autoUpgrade.flags =
    [ "--update-input" "nixpkgs" "--recreate-lock-file" ];

  time.timeZone = "America/New_York";

  networking.useDHCP = lib.mkDefault true;

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  users.groups.tls = { };
  security.pki.certificateFiles = [
    "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ./juniper-berry-root-ca.pem
  ];
}
