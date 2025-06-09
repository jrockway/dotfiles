{ pkgs, ... }: {
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  environment.systemPackages = with pkgs; [ mg pciutils mmc-utils usbutils ];
}
