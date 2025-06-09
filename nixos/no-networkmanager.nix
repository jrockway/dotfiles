{ config, lib, pkgs, ... }: {
  networking.networkmanager.enable = false;
  networking.useDHCP = false;

  systemd.network = {
    enable = true;
    wait-online = {
      timeout = 5;
      anyInterface = true;
    };
  };
}
