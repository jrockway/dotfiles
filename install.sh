#!/usr/bin/env bash

# This is only for installing things in a Github Codespace type thing.  You will want system-wide
# nix in most real places.

set -x

sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon
. ~/.nix-profile/etc/profile.d/nix.sh

mkdir -p ~/.config
ln -sf $PWD/config/nix ~/.config/nix
ln -sf $PWD/config/home-manager ~/.config/home-manager

mv ~/.bashrc ~/.bashrc-orig
mv ~/.profile ~/.profile-orig
mv ~/.bash_logout ~/.bash_logout-orig

nix run home-manager/release-25.11 -- switch
