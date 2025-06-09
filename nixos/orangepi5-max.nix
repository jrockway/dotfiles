{ config, lib, pkgs, ... }: {
  services.chrony.extraConfig = "makestep 0.1 3";
  systemd.network.networks."enP3p49s0" = {
    matchConfig.Name = "enP3p49s0";
    networkConfig.DHCP = "ipv4";
    linkConfig.RequiredForOnline = "yes";
  };
  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXROOT";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/NIXBOOT";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  swapDevices = [ ];
}
