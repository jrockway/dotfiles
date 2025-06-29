{ config, lib, pkgs, ... }: {
  sops.secrets.jrockway-password.neededForUsers = true;

  users.users.jrockway = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzAhJaA8i7e1zomE5m+eTrp4GRQpYt0PDkG2RLTouiE"
    ];
    hashedPasswordFile = config.sops.secrets.jrockway-password.path;
  };
}
