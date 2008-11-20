#!/bin/bash

cd $HOME;

export TZ="America/Chicago"

eval `gpg-agent --pinentry-program /usr/bin/pinentry-gtk2 --enable-ssh-support --write-env-file $HOME/.gpg-agent-info --daemon --use-standard-socket`
. $HOME/.gpg-agent-info

scim -d
urxvtd -q -f -o
xmodmap ~/.xmodmaprc
xbindkeys
xinput set-button-map Mouse0 1 8 3 4 5 6 7 2 9
pidof emacs || emacs --daemon &
unclutter -idle 1 &
xmms2-launcher &
. $HOME/utils/irc-ssh.sh

offlineimap -u Noninteractive.Quiet &
mount /mnt/bingo &

exec xmonad | xmobar ~/.xmonad/mobar.conf
