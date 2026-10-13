#!/bin/sh
# EC2 user-data for the "aws" host in ./flake.nix. The official NixOS AMI's
# amazon-init service runs user-data as a script (because it starts with "#!"),
# as root, once network-online is reached. Nothing secret belongs in here:
# user-data is readable by anyone who can describe the instance.
set -eu

export NIX_CONFIG="extra-experimental-features = nix-command flakes"
nixos-rebuild switch --flake 'github:jrockway/dotfiles?dir=nixos#aws'

# Stage the dotfiles so home-manager is one command after first login. The
# switch itself can't run here: the home-manager config decrypts sops secrets
# with ~/.ssh/id_ed25519, which isn't on the box yet.
#   nix run home-manager/release-26.05 -- switch
if [ ! -d /home/jrockway/.dotfiles ]; then
  sudo -Hu jrockway git clone https://github.com/jrockway/dotfiles /home/jrockway/.dotfiles
  sudo -Hu jrockway mkdir -p /home/jrockway/.config
  sudo -Hu jrockway ln -sfn /home/jrockway/.dotfiles/config/home-manager /home/jrockway/.config/home-manager
fi
