# jrockway's login password, decrypted by sops-nix with the host's ssh host key.
# Only import this on hosts whose age key is listed in ../.sops.yaml: a host that
# can't decrypt the secret fails activation (neededForUsers runs before users
# are created). Fresh EC2 instances generate their host key at first boot, so
# they can't import it.
{ config, ... }:
{
  sops.secrets.jrockway-password.neededForUsers = true;
  users.users.jrockway.hashedPasswordFile = config.sops.secrets.jrockway-password.path;
}
