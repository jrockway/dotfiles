{ pkgs, ... }: {
  boot.kernel.sysctl = { "kernel.dmesg_restrict" = 0; };
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  environment.systemPackages = with pkgs; [ mg pciutils mmc-utils usbutils ];
}
