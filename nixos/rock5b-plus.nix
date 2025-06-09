{ config, lib, pkgs, ... }: {
  services.chrony.extraConfig = "makestep 0.1 3";
  systemd.network.networks."enP4p65s0" = {
    matchConfig.Name = "enP4p65s0";
    networkConfig.DHCP = "ipv4";
    linkConfig.RequiredForOnline = "yes";
  };
}
