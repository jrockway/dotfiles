{ pkgs, lib, ... }: {
  isoImage.squashfsCompression = "zstd -Xcompression-level 3";
  boot = {
    kernelPackages = pkgs.linuxPackages_6_14;
    kernelModules = [ "panthor" "mmc_block" "input_leds" "dw_mmc_rockchip" ];
    kernelParams = [ "console=ttyS2,1500000n8" ];
    initrd.availableKernelModules = lib.mkForce [
      "dw_mmc_rockchip"
      "hid"
      "input_leds"
      "mmc_block"
      "nvme"
      "panthor"
      "sdhci_of_dwcmshc"
    ];
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };
}
