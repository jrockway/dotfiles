{ ... }: {
  services.chrony = {
    initstepslew.enabled = false; # makestep is better
    enableRTCTrimming = false;
    extraConfig = ''
      makestep 0.1 3
      rtcsync
    '';
  };
  systemd.network.networks."enP4p65s0" = {
    matchConfig.Name = "enP4p65s0";
    networkConfig.DHCP = "ipv4";
    linkConfig.RequiredForOnline = "yes";
  };
  # turn off VERY BRIGHT status LED
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="leds", KERNEL=="blue:status", ATTR{trigger}="none"
  '';
}
