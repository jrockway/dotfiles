# EC2 host. This is not baked into an AMI: launch the official NixOS AMI
# (AWS account 427812963091, published weekly for x86_64 and arm64) with
# ../aws-user-data.sh as the instance's user-data, and amazon-init switches
# to this configuration on first boot. Reapply later with
#   sudo nixos-rebuild switch --flake 'github:jrockway/dotfiles?dir=nixos#aws'
#
# Console: Launch instance -> Browse more AMIs -> Community AMIs -> search
# "nixos/", architecture arm64 -> instance type t4g/m7g/c7g -> root volume
# 40 GB or more (the AMI's own disk is 4 GB and the closure won't fit) ->
# Advanced details -> User data: paste ../aws-user-data.sh.
#
# CLI (the root device name must match the AMI's, so read it too):
#   read -r ami root < <(aws ec2 describe-images --owners 427812963091 \
#     --filters 'Name=name,Values=nixos/*' 'Name=architecture,Values=arm64' \
#     --query 'sort_by(Images, &CreationDate)[-1].[ImageId,RootDeviceName]' \
#     --output text)
#   aws ec2 run-instances --image-id "$ami" --instance-type m7g.large \
#     --key-name <keypair> --security-group-ids <sg> \
#     --block-device-mappings "DeviceName=$root,Ebs={VolumeSize=40}" \
#     --user-data file://nixos/aws-user-data.sh
#
# Official AMIs are garbage-collected after about 90 days, so resolve the AMI
# at launch time rather than pinning an ID in a launch template.
{ pkgs, ... }:
{
  networking.hostName = "aws";
  nixpkgs.hostPlatform = "aarch64-linux";
  system.stateVersion = "25.05";

  # base.nix enables autoUpgrade without naming a flake. On EC2 that would
  # rebuild the stock /etc/nixos/configuration.nix the AMI shipped with and
  # revert this configuration nightly, so point it at this flake.
  system.autoUpgrade.flake = "github:jrockway/dotfiles?dir=nixos#aws";

  # VS Code remote and other dynamically linked binaries.
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ glibc ];
}
