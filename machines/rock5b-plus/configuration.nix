# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_6_15;
  boot.kernelModules = [ "panthor" ];
  boot.kernelParams = [ "console=ttyS2,1500000n8" ];
  boot.initrd.availableKernelModules = lib.mkForce [
    "adc_keys"
    "bridge"
    "cdrom"
    "cmdlinepart"
    "dax"
    "display_connector"
    "dm_mod"
    "dmi_sysfs"
    "drm_exec"
    "drm_gpuvm"
    "dw_mmc_rockchip"
    "ffa_core"
    "gpu_sched"
    "hantro_vpu"
    "iso9660"
    "isofs"
    "llc"
    "macvlan"
    "mc"
    "mmc_block"
    "nls_cp437"
    "nls_iso8859_1"
    "nvme"
    "nvme_auth"
    "nvme_core"
    "optee"
    "panthor"
    "pci_endpoint_test"
    "phy_rockchip_naneng_combphy"
    "phy_rockchip_usbdp"
    "polyval_ce"
    "polyval_generic"
    "pwm_fan"
    "r8169"
    "rfkill"
    "rfkill_gpio"
    "rk805_pwrkey"
    "rockchip_dfi"
    "rockchip_rga"
    "rockchip_saradc"
    "rockchip_thermal"
    "rpmb_core"
    "rtc_hym8563"
    "sdhci_of_dwcmshc"
    "sm4"
    "spi_rockchip_sfc"
    "stp"
    "tap"
    "typec"
    "uas"
    "uio"
    "uio_pdrv_genirq"
    "v4l2_h264"
    "v4l2_jpeg"
    "v4l2_mem2mem"
    "v4l2_vp9"
    "videobuf2_common"
    "videobuf2_dma_contig"
    "videobuf2_dma_sg"
    "videobuf2_memops"
    "videobuf2_v4l2"
    "videodev"
  ];
  boot.kernel.sysctl = { "kernel.dmesg_restrict" = 0; };

  networking.hostName = "berry-rock5bplus-0"; # Define your hostname.
  networking.networkmanager.enable = false;
  networking.useDHCP = false;
  systemd.network = {
    enable = true;
    wait-online = {
      timeout = 5;
      anyInterface = true;
    };
    networks."enP4p65s0" = {
      matchConfig.Name = "enP4p65s0";
      networkConfig.DHCP = "ipv4";
      linkConfig.RequiredForOnline = "yes";
    };
  };
  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";

  users.users.jrockway = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzAhJaA8i7e1zomE5m+eTrp4GRQpYt0PDkG2RLTouiE"
    ];
  };

  services.openssh.enable = true;
  services.tailscale.enable = true;
  services.prometheus.exporters.node = {
    openFirewall = true;
    enable = true;
  };
  services.chrony.enable = true;
  services.timesyncd.enable = false;

  security.sudo.wheelNeedsPassword = false;
  nix.gc.automatic = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = false;

  environment.systemPackages = with pkgs; [ mg tailscale gitFull home-manager ];

  system.copySystemConfiguration = true;
  system.stateVersion = "25.05"; # Did you read the comment?
}
